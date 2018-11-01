{-# language LinearTypes #-}

module Data.Semigroup.Linear
  ( LinearSemigroup(..)
  ) where

import Data.Primitive (Prim)
import Data.Linear.Types (Ref,Struct,Indirection,Unrestricted(..))
import qualified Data.Linear.Ref as R
import qualified Data.Linear.Struct as S

class LinearSemigroup m where
  linearAppend :: m ->. m ->. m

instance (LinearSemigroup m, Indirection m) => LinearSemigroup (Ref m) where
  linearAppend a b = R.modify_ a (\x -> linearAppend x (R.deallocate b))

instance (Semigroup m, Prim m) => LinearSemigroup (Struct m) where
  linearAppend a b = structLinearSemigroupHelper (S.read a) (S.read b)

structLinearSemigroupHelper :: (Semigroup a, Prim a) => (Struct a, Unrestricted a) ->. (Struct a, Unrestricted a) ->. Struct a
structLinearSemigroupHelper (x,Unrestricted a) (y,Unrestricted b) =
  S.write (S.graft x (S.deallocate y)) (a <> b)

