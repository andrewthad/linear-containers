{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}

module Linear.Reference
  ( ignore
  , read
  , statically
  , statically_
  , dynamically_
  , modify
  , allocate
  , deallocate
  ) where

import Prelude hiding (read)

import Data.Primitive (Addr)
import Linear.Types (Mode(..),Unrestricted(..),Object,Token,Referential(..))
import Linear.Unsafe (Reference(..))
import Data.Proxy (Proxy(..))

import qualified Linear.Unsafe as U
import qualified Linear.Types as L
import qualified Linear.Class as C

ignore :: Reference f 'Static ->. Token
ignore (Reference _ t) = t

read :: Object f
  =>  Reference f 'Static
  ->. (Token, f 'Static)
read (Reference addr t0) = L.peek addr t0

statically_ :: Object f
  =>  Reference f 'Dynamic
  ->. (f 'Static ->. Token ->. Token)
  ->. Reference f 'Dynamic
statically_ (Reference addr t0) f =
  Reference addr (C.uncurry (C.flip f) (L.peek addr t0))

statically ::
      Reference f 'Dynamic
  ->. (Reference f 'Static ->. (Token, Unrestricted a))
  ->. (Reference f 'Dynamic, Unrestricted a)
statically (Reference addr t0) f = staticallyStep addr (f (Reference addr t0))

staticallyStep :: Addr -> (Token, Unrestricted a) ->. (Reference f 'Dynamic, Unrestricted a)
staticallyStep !addr (!t1,u) = (Reference addr t1, u)

dynamically_ :: Object f
  =>  Reference f 'Static
  ->. (f 'Dynamic ->. f 'Dynamic)
  ->. Token
dynamically_ (Reference addr t0) f =
  C.uncurry (C.flip (L.poke addr)) (C.second f (L.peek addr t0))

allocate :: forall f. Object f
  =>  f 'Dynamic
  ->. Token
  ->. Reference f 'Dynamic
allocate a !t0 = U.withAllocatedBytes (L.size (Proxy :: Proxy f)) t0
  (\addr t1 -> Reference addr (L.poke addr a t1))

allocate' :: forall f. Referential f
  =>  f 'Dynamic
  ->. Reference f 'Dynamic
allocate' a = C.uncurry (C.flip allocate) (L.exhume a)

deallocate :: forall f. Object f
  =>  Reference f 'Dynamic
  ->. (Token, f 'Dynamic)
deallocate (Reference addr t0) = C.uncurry
  (\t1 a -> U.withDeallocate addr t1 (\t2 -> (t2, a)))
  (L.peek addr t0)

-- | This can be used to modify either a static reference or
--   a dynamic reference.
modify :: Object f
  =>  Reference f m
  ->. (f 'Dynamic ->. f 'Dynamic)
  ->. Reference f m
modify (Reference addr t0) f =
  Reference addr (C.uncurry (C.flip (L.poke addr)) (C.second f (L.peek addr t0)))

