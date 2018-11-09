{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language LinearTypes #-}
{-# language ScopedTypeVariables #-}

import Test.Tasty (testGroup,defaultMain)
import GHC.Int (Int32(I32#))
import Test.QuickCheck ((===),(==>))
import Linear.Types (PrimObject(..),Mode(..),Token,getPrimObject)
import Linear.Class (Unrestricted(..),(<>.))

import qualified GHC.Exts as E
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC

import qualified Linear.Reference as Ref
import qualified Linear.List as List
import qualified Linear.Class as C

main :: IO ()
main = defaultMain $ testGroup "Linear"
  [ testGroup "List"
    [ TQC.testProperty "foldl (+) 0 (replicate n x) = n * x" $ \(QC.NonNegative (QC.Small (n :: Int32))) (x :: Int32) ->
        C.getUnrestricted (Ref.run (\t -> C.second C.move (List.foldl linearAdd (PrimObject 0) (List.replicate (fromIntegral n) (PrimObject x) t))))
        ===
        PrimObject (n * x)
    , TQC.testProperty "append xs (append ys zs) = append (append xs ys) zs" $ \(xs :: [Int32]) ys zs ->
        let C.Unrestricted (as,bs) = Ref.run (\t -> associativityAppend (C.coappend (C.coappend3 t)) (map PrimObject xs) (map PrimObject ys) (map PrimObject zs))
         in map (\c -> getPrimObject c) as === map (\c -> getPrimObject c) bs
    ]
  ]

associativityAppend ::
      ((Token,Token,Token),(Token,Token,Token))
  ->. [PrimObject Int32 'Dynamic]
  ->  [PrimObject Int32 'Dynamic]
  ->  [PrimObject Int32 'Dynamic]
  ->  (Token, Unrestricted ([PrimObject Int32 'Dynamic], [PrimObject Int32 'Dynamic]))
associativityAppend ((t1,t2,t3),(t4,t5,t6)) xs ys zs = tweak
  (List.toNonlinear (List.append (List.fromNonlinear xs t1) (List.append (List.fromNonlinear ys t2) (List.fromNonlinear zs t3))))
  (List.toNonlinear (List.append (List.append (List.fromNonlinear xs t4) (List.fromNonlinear ys t5)) (List.fromNonlinear zs t6)))
  where
  tweak :: (Token,[PrimObject Int32 'Dynamic]) ->. (Token,[PrimObject Int32 'Dynamic]) ->. (Token, Unrestricted ([PrimObject Int32 'Dynamic],[PrimObject Int32 'Dynamic]))
  tweak (a,as) (b,bs) = (a <>. b, C.move (as,bs))

linearAdd :: PrimObject Int32 m ->. PrimObject Int32 m ->. PrimObject Int32 m
linearAdd (PrimObject (I32# x)) (PrimObject (I32# y)) = PrimObject (I32# (E.narrow32Int# (x E.+# y)))


