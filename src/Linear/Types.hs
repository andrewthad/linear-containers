{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Linear.Types
  ( Reference
  , Token
  , Static
  , Object(..)
  , Referential(..)
  , PrimObject(..)
  ) where

import Data.Kind (Type)
import Foreign.Ptr 
import Linear.Class ((<>.),coappend)
import Linear.Unsafe (Reference(..),Token(..),Static(..))
import Data.Primitive (Addr(..),Prim)
import Data.Proxy (Proxy)
import Data.Int (Int32)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (RealWorld,State#,Addr#)

import qualified GHC.Exts as E
import qualified Data.Primitive as PM
import qualified Linear.Class as C
import qualified Linear.Unsafe as U

-- | Object types can be serialized to the unmanaged heap. This includes
--   types containing a 'Reference' but excludes types containing functions.
class Object a where 
  -- | The size of the object in bytes.
  size :: Proxy a -> Int
  -- | Write the object to the specified memory address.
  poke :: Addr -> a ->. Token ->. Token
  -- | Read the object from the specified memory address.
  peek :: Addr -> Token ->. (Token, a)
  -- | Discard all linear components of the static object.
  ignore :: Static a ->. Token ->. Token

class Object a => Destroy a where
  -- | Free the object and any of its child objects from memory.
  destroy :: a ->. Token ->. Token

-- | Typeclass for types that include a reference. This means
--   that we can inject a sequence token into the data and
--   enforce a particular ordering of operations. In practice,
--   this gives the user access the additional conveniences when
--   working with these types.
class Object a => Referential a where
  -- | Combine the token with a reference inside of the object.
  inhume :: Token ->. a ->. a
  -- | Duplicate a token that exists in the object.
  exhume :: a ->. (Token, a)

instance Object () where
  size _ = 0
  poke _ () t = t
  peek _ t = (t,())
  ignore (Static ()) t = t

instance Object (Reference a) where
  size _ = PM.sizeOf (undefined :: Addr)
  {-# INLINE size #-}
  peek = unsafeCoerce referencePeek
  {-# INLINE peek #-}
  poke = unsafeCoerce referencePoke
  {-# INLINE poke #-}
  ignore (Static (Reference _ t0)) t1 = t0 <>. t1
  {-# INLINE ignore #-}

instance Destroy a => Destroy (Reference a) where
  destroy (Reference addr t0) t = C.uncurry
    (\t1 a -> U.withDeallocate addr t1 (\t2 -> destroy a t2))
    (peek @a addr (t0 <>. t))

instance Referential (Reference f) where
  inhume a (Reference addr b) = Reference addr (a <>. b)
  exhume (Reference addr a) = C.uncurry (\b c -> (b, Reference addr c)) (coappend a)

-- | Recover an 'Object' instance from a 'Prim' instance.
--   This currently requires using @unsafeCoerce@ internally.
newtype PrimObject :: Type -> Type where
  PrimObject :: a ->. PrimObject a

instance (Prim a, C.Comonoid a) => Object (PrimObject a) where
  size _ = PM.sizeOf (undefined :: a)
  poke = unsafeCoerce (primPoke @a)
  peek = unsafeCoerce (primPeek @a)
  ignore (Static (PrimObject x)) = ununit (C.coempty x)

instance Object Int32 where
  size _ = PM.sizeOf (undefined :: Int32)
  poke = unsafeCoerce (primPoke @Int32)
  peek = unsafeCoerce (primPeek @Int32)
  ignore (Static x) = ununit (C.coempty x)

ununit :: () ->. a ->. a
ununit () a = a

{-# INLINE referencePeek #-}
referencePeek :: Addr -> Token -> (Token, Reference a)
referencePeek (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> (Token s1, Reference a (Token s1))

{-# INLINE referencePoke #-}
referencePoke :: Addr -> Reference a -> Token -> Token
referencePoke (Addr addr) (Reference a (Token s0)) (Token s1) =
  case PM.writeOffAddr# addr 0# a (E.joinState# s0 s1) of
    s2 -> Token s2

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
primPoke :: Prim a => Addr -> PrimObject a -> Token -> Token
primPoke (Addr addr) (PrimObject a) (Token s0) = Token (PM.writeOffAddr# addr 0# a s0)

{-# INLINE primPeek #-}
primPeek :: Prim a => Addr -> Token -> (Token, PrimObject a)
primPeek (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> (Token s1, PrimObject a)


