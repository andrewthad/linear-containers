{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language DataKinds #-}
{-# language InstanceSigs #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Linear.Types
  ( Mode(..)
  , Reference
  , Token
  , Object(..)
  , Referential(..)
  , PrimObject(..)
  , peek
  , getPrimObject
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

-- | Object types can be serialized to the unmanaged heap. This includes
--   types containing a 'Reference' but excludes types containing functions.
class Object (f :: Mode -> Type) where 
  -- | The size of the object in bytes.
  size :: Proxy f -> Int
  -- | Write the object to the specified memory address.
  poke :: Addr -> f m ->. Token ->. Token
  -- | Read the object from the specified memory address. This is
  --   CPSed so that @m@ can be appropriately quantified. The utility
  --   function 'peek' is provided to recover the behavior one often
  --   wants.
  peeking :: forall a. Addr -> Token ->. (Token ->. (forall m. (f m)) ->. a) ->. a
  -- | Discard all linear components of the static object.
  forget :: f 'Static ->. Token ->. Token

peek :: forall (f :: Mode -> Type) (m :: Mode). Object f => Addr -> Token ->. (Token, f m)
peek a t = peeking @f @(Token,f m) a t (peekHelp @f @m)

peekHelp :: forall (f :: Mode -> Type) (n :: Mode). Token ->. (forall m. (f m)) ->. (Token,f n)
peekHelp t x = (t,x @n)

-- | Typeclass for types that include a reference. This means
--   that we can inject a sequence token into the data and
--   enforce a particular ordering of operations. In practice,
--   this gives the user access the additional conveniences when
--   working with these types.
class Object f => Referential (f :: Mode -> Type) where
  -- | Combine the token with a reference inside of the object.
  inhume :: Token ->. f m ->. f m
  -- | Duplicate a token that exists in the object.
  exhume :: f m ->. (Token, f m)

instance Object (Reference f) where
  size _ = PM.sizeOf (undefined :: Addr)
  {-# INLINE size #-}
  peeking :: forall b. Addr -> Token ->. (Token ->. (forall m. Reference f m) ->. b) ->. b
  peeking x y z = referencePeek' @f @b z x y
  {-# INLINE peeking #-}
  poke = unsafeCoerce referencePoke
  {-# INLINE poke #-}
  forget (Reference _ t0) t1 = t0 <>. t1
  {-# INLINE forget #-}

instance Referential (Reference f) where
  inhume a (Reference addr b) = Reference addr (a <>. b)
  exhume (Reference addr a) = C.uncurry (\b c -> (b, Reference addr c)) (coappend a)

-- | Recover an 'Object' instance from a 'Prim' instance.
--   This currently requires using @unsafeCoerce@ internally.
newtype PrimObject :: Type -> Mode -> Type where
  PrimObject :: a ->. PrimObject a m
  deriving (Eq,Show)

instance C.Movable a => C.Movable (PrimObject a m) where
  move (PrimObject a) = movePrimObjectHelp (C.move a)

movePrimObjectHelp :: C.Unrestricted a ->. C.Unrestricted (PrimObject a m)
movePrimObjectHelp (C.Unrestricted a') = C.Unrestricted (PrimObject a')

instance (Prim a, C.Comonoid a) => Object (PrimObject a) where
  size _ = PM.sizeOf (undefined :: a)
  poke = unsafeCoerce (primPoke @a)
  peeking :: forall b. Addr -> Token ->. (Token ->. (forall m. PrimObject a m) ->. b) ->. b
  peeking x y z = (primPeek' @a @b z x y)
  forget (PrimObject x) = ununit (C.coempty x)

ununit :: () ->. a ->. a
ununit () a = a

{-# INLINE referencePeek' #-}
referencePeek' :: forall f b. (Token ->. (forall m. Reference f m) ->. b) ->. Addr -> Token ->. b
referencePeek' f = linearUnsafeCoerce (referencePeek @f @b f)

{-# INLINE referencePeek #-}
referencePeek :: forall f b. (Token ->. (forall m. Reference f m) ->. b) ->. Addr -> Token -> b
referencePeek f (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> f (Token s1) (Reference a (Token s1))

{-# INLINE referencePoke #-}
referencePoke :: Addr -> Reference f m -> Token -> Token
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
primPoke :: Prim a => Addr -> PrimObject a m -> Token -> Token
primPoke (Addr addr) (PrimObject a) (Token s0) = Token (PM.writeOffAddr# addr 0# a s0)

{-# INLINE primPeek' #-}
primPeek' :: forall a b. Prim a => (Token ->. (forall m. PrimObject a m) ->. b) ->. Addr -> Token ->. b
primPeek' f = linearUnsafeCoerce (primPeek @a @b f)

{-# INLINE primPeek #-}
primPeek :: forall a b. Prim a => (Token ->. (forall m. PrimObject a m) ->. b) ->. Addr -> Token -> b
primPeek f (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> f (Token s1) (PrimObject a)

linearUnsafeCoerce :: a ->. b
linearUnsafeCoerce = unsafeCoerce unsafeCoerce

getPrimObject :: PrimObject a m ->. a
getPrimObject (PrimObject a) = a

