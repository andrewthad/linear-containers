{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Linear.Types
  ( Unrestricted(..)
  , Mode(..)
  , Reference
  , Token
  , Object(..)
  , Referential(..)
  , PrimObject(..)
  ) where

import Data.Kind (Type)
import Foreign.Ptr 
import Linear.Class ((<>.),coappend)
import Linear.Unsafe (Reference(..),Mode(..),Token(..))
import Data.Primitive (Addr(..),Prim)
import Data.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (RealWorld,State#,Addr#)

import qualified GHC.Exts as E
import qualified Data.Primitive as PM
import qualified Linear.Class as C

class Object (f :: Mode -> Type) where 
  size :: Proxy f -> Int
  poke :: Addr -> f m ->. Token ->. Token
  peek :: Addr -> Token ->. (Token, f m)
  forget :: f 'Static ->. Token ->. Token

-- | Typeclass for types that include a reference. This means
--   that we can inject a sequence token into the data and
--   enforce a particular ordering of operations. In practice,
--   this gives the user access the additional conveniences when
--   working with these types.
class Object f => Referential (f :: Mode -> Type) where
  inhume :: Token ->. f m ->. f m
  exhume :: f m ->. (Token, f m)

instance Object (Reference f) where
  size _ = PM.sizeOf (undefined :: Addr)
  {-# INLINE size #-}
  peek = unsafeCoerce referencePeek
  {-# INLINE peek #-}
  forget (Reference _ t0) t1 = t0 <>. t1
  {-# INLINE forget #-}

instance Referential (Reference f) where
  inhume a (Reference addr b) = Reference addr (a <>. b)
  exhume (Reference addr a) = C.uncurry (\b c -> (b, Reference addr c)) (coappend a)

data Unrestricted a where
  Unrestricted :: a -> Unrestricted a
  deriving (Show)

-- | Recover an 'Object' instance from a 'Prim' instance.
newtype PrimObject :: Type -> Mode -> Type where
  PrimObject :: a ->. PrimObject a m

instance (Prim a, C.Comonoid a) => Object (PrimObject a) where
  size _ = PM.sizeOf (undefined :: a)
  poke = unsafeCoerce (primPoke @a)
  peek = unsafeCoerce (primPeek @a)
  forget (PrimObject x) = ununit (C.coempty x)

ununit :: () ->. a ->. a
ununit () a = a

{-# INLINE referencePeek #-}
referencePeek :: Addr -> Token -> (Token, Reference f m)
referencePeek (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> (Token s1, Reference a (Token s1))

-- linearUnsafeCoerce :: a ->. b
-- linearUnsafeCoerce = unsafeCoerce unsafeCoerce

-- {-# INLINE referencePeekCps #-}
-- referencePeekCps :: Addr# -> State# RealWorld ->. (Addr# -> State# RealWorld ->. State# RealWorld ->. a) ->. a
-- referencePeekCps = unsafeCoerce referencePeekCpsNonlinear
-- 
-- {-# INLINE referencePeekCpsNonlinear #-}
-- referencePeekCpsNonlinear :: Addr# -> State# RealWorld -> (Addr# -> State# RealWorld -> State# RealWorld -> a) -> a
-- referencePeekCpsNonlinear addr s0 f = case E.readAddrOffAddr# addr 0# s0 of
--   (# s1, a #) -> f a s1 s1

{-# INLINE primPoke #-}
primPoke :: Prim a => Addr -> PrimObject a m -> Token -> Token
primPoke (Addr addr) (PrimObject a) (Token s0) = Token (PM.writeOffAddr# addr 0# a s0)

{-# INLINE primPeek #-}
primPeek :: Prim a => Addr -> Token -> (Token, PrimObject a m)
primPeek (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> (Token s1, PrimObject a)


