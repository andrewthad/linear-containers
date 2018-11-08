{-# language MagicHash #-}
{-# language LinearTypes #-}
{-# language ScopedTypeVariables #-}

import Test.Tasty (testGroup,defaultMain)
import GHC.Int (Int32(I32#))
import Test.QuickCheck ((===),(==>))

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
        C.getUnrestricted (Ref.run (\t -> C.second C.move (List.foldl linearAdd 0 (List.replicate (fromIntegral n) x t))))
        ===
        (n * x)
    ]
  ]

linearAdd :: Int32 ->. Int32 ->. Int32
linearAdd (I32# x) (I32# y) = I32# (E.narrow32Int# (x E.+# y))

