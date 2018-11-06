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
  , Action
  , Object(..)
  , Referential(..)
  , PrimObject(..)
  ) where

import Data.Kind (Type)
import Foreign.Ptr 
import Linear.Class ((<>.),coappend)
import Linear.Unsafe (Reference(..),Mode(..),Token(..),Action(..))
import Data.Primitive (Addr(..),Prim)
import Data.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (RealWorld,State#,Addr#)

import qualified GHC.Exts as E
import qualified Data.Primitive as PM
import qualified Linear.Class as C

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
  ignore :: a ->. Action 'Static ->. Action 'Static

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

instance Object (Reference a) where
  size _ = PM.sizeOf (undefined :: Addr)
  {-# INLINE size #-}
  peek = unsafeCoerce referencePeek
  {-# INLINE peek #-}
  ignore (Reference _ t0) t1 = combineActionToken t0 t1
  {-# INLINE ignore #-}

combineActionToken :: Token ->. Action m ->. Action m
combineActionToken (Token s0) (Action s1) = Action (E.joinState# s0 s1)

instance Referential (Reference f) where
  inhume a (Reference addr b) = Reference addr (a <>. b)
  exhume (Reference addr a) = C.uncurry (\b c -> (b, Reference addr c)) (coappend a)

-- | An unrestricted value.
data Unrestricted a where
  Unrestricted :: a -> Unrestricted a
  deriving (Show)

-- | Recover an 'Object' instance from a 'Prim' instance.
--   This currently requires using @unsafeCoerce@ internally.
newtype PrimObject :: Type -> Type where
  PrimObject :: a ->. PrimObject a

instance (Prim a, C.Comonoid a) => Object (PrimObject a) where
  size _ = PM.sizeOf (undefined :: a)
  poke = unsafeCoerce (primPoke @a)
  peek = unsafeCoerce (primPeek @a)
  ignore (PrimObject x) = ununit (C.coempty x)

ununit :: () ->. a ->. a
ununit () a = a

{-# INLINE referencePeek #-}
referencePeek :: Addr -> Token -> (Token, Reference a)
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
primPoke :: Prim a => Addr -> PrimObject a -> Token -> Token
primPoke (Addr addr) (PrimObject a) (Token s0) = Token (PM.writeOffAddr# addr 0# a s0)

{-# INLINE primPeek #-}
primPeek :: Prim a => Addr -> Token -> (Token, PrimObject a)
primPeek (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> (Token s1, PrimObject a)


