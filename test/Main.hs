{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language LinearTypes #-}
{-# language ScopedTypeVariables #-}

import Test.Tasty (testGroup,defaultMain)
import GHC.Int (Int32(I32#))
import Test.QuickCheck ((===),(==>))
import Linear.Types (PrimObject(..),Mode(..),Token)
import Linear.Class (Unrestricted(..),(<>.),(<*>.),(<$>.))

import Linear.Reader (reader,runReader)

import qualified GHC.Exts as E
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC

import qualified Linear.Reference as Ref
import qualified Linear.List as List
import qualified Linear.Class as C
import qualified Linear.Stack as S
import qualified Linear.Slate as SL

main :: IO ()
main = defaultMain $ testGroup "Linear"
  [ testGroup "primitives"
    [ TQC.testProperty "PrimObject" $ \(i :: Int32) ->
        C.getUnrestricted (Ref.run (\t0 -> unPrimObject (Ref.deallocate (Ref.allocate (PrimObject i t0)))))
        ===
        i
    , TQC.testProperty "Reference" $ \(i :: Int32) ->
        C.getUnrestricted (Ref.run (\t0 -> unPrimObject (Ref.deallocate (Ref.deallocate (Ref.allocate (Ref.allocate (PrimObject i t0)))))))
        ===
        i
    ]
  , testGroup "List"
    [ TQC.testProperty "toPrim . fromPrim = id" $ \(xs :: [Int32]) ->
        C.getUnrestricted (Ref.run (\t0 -> C.second C.move (List.toPrim (List.fromPrim xs t0))))
        ===
        xs
    , TQC.testProperty "foldl (+) 0 (replicate n x) = n * x" $ \(QC.NonNegative (QC.Small (n :: Int32))) (x :: Int32) ->
        C.getUnrestricted (Ref.run (\t0 -> C.uncurry (\t1 t2 -> C.uncurry (\tx (PrimObject r ty) -> (tx <>. ty, Unrestricted r)) (List.foldl linearAdd (PrimObject 0 t1) (List.replicate (fromIntegral n) x t2))) (C.coappend t0)))
        ===
        (n * x)
    , TQC.testProperty "append xs (append ys zs) = append (append xs ys) zs" $ \(xs :: [Int32]) ys zs ->
        let C.Unrestricted (as,bs) = Ref.run (\t -> associativityAppend (C.coappend (C.coappend3 t)) xs ys zs) in as === bs
    ]
  , testGroup "Slate"
    [ TQC.testProperty "foldl (+) 0 (push empty x) == x" $ \(v :: Int32) ->
        C.getUnrestricted (Ref.run (\t0 -> C.uncurry (\(t1,t8) (t2,t9) -> C.uncurry (\t3 (PrimObject v' t4) -> (t3 <>. t4, C.move v')) (SL.foldl linearAdd (PrimObject 0 t9) (SL.push (SL.allocate 1 t1) (PrimObject v (t2 <>. t8))))) (C.coappend (C.coappend t0))))
        ===
        v
    , TQC.testProperty "pop (push s v) == (s,v)" $ \(v :: Int32) ->
        C.getUnrestricted (Ref.run (\t0 -> C.uncurry (\t1 t2 -> C.uncurry (\s (PrimObject v' t3) -> (SL.deallocateEmpty s <>. t3, C.move v')) (SL.pop (SL.push (SL.allocate 1 t1) (PrimObject v t2)))) (C.coappend t0)))
        ===
        v
    ]
  , testGroup "Stack"
    [ TQC.testProperty "foldl (+) 0 (push x (push y (push z (empty t)))) = x + y + z" $ \(x :: Int32) y z ->
        C.getUnrestricted
          ( Ref.run
            (\t0 -> stackFold (runReader ((,,,,) <$>. reader C.id <*>. reader C.id <*>. reader (PrimObject x) <*>. reader (PrimObject y) <*>. reader (PrimObject z)) t0))
          )
        ===
        (x + y + z)
    ]
  ]

stackFold :: (Token,Token,PrimObject Int32 'Dynamic, PrimObject Int32 'Dynamic, PrimObject Int32 'Dynamic) ->. (Token, Unrestricted Int32)
stackFold (t0,t1,x,y,z) = tweak (S.foldl linearAdd (PrimObject 0 t0) (S.push (S.push (S.push (S.empty t1) z) y) x))
  where
  tweak :: (Token,PrimObject Int32 'Dynamic) ->. (Token,Unrestricted Int32)
  tweak (t2,PrimObject x t3) = (t2 <>. t3, C.move x)

associativityAppend ::
      ((Token,Token,Token),(Token,Token,Token))
  ->. [Int32]
  ->  [Int32]
  ->  [Int32]
  ->  (Token, Unrestricted ([Int32], [Int32]))
associativityAppend ((t1,t2,t3),(t4,t5,t6)) xs ys zs = tweak
  (List.toPrim (List.append (List.fromPrim xs t1) (List.append (List.fromPrim ys t2) (List.fromPrim zs t3))))
  (List.toPrim (List.append (List.append (List.fromPrim xs t4) (List.fromPrim ys t5)) (List.fromPrim zs t6)))
  where
  tweak :: (Token,[Int32]) ->. (Token,[Int32]) ->. (Token, Unrestricted ([Int32],[Int32]))
  tweak (a,as) (b,bs) = (a <>. b, C.move (as,bs))

linearAdd :: PrimObject Int32 m ->. PrimObject Int32 m ->. PrimObject Int32 m
linearAdd (PrimObject (I32# x) t0) (PrimObject (I32# y) t1) = PrimObject (I32# (E.narrow32Int# (x E.+# y))) (t0 <>. t1)

unPrimObject :: PrimObject a 'Dynamic ->. (Token,Unrestricted a)
unPrimObject (PrimObject a t) = (t,Unrestricted a)

