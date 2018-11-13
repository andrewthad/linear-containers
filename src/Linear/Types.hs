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

-- | Object types can be serialized to the unmanaged heap. This includes
--   types containing a 'Reference' but excludes types containing functions.
--   Additionally, the object must transitively contain at least one token
--   or reference. This means that that we can inject a sequence token into
--   the data and enforce a particular ordering of operations. In practice,
--   this gives the user access the additional conveniences when
--   working with these types.
class Object (f :: Mode -> Type) where 
  -- | The size of the object in bytes.
  size :: Proxy f -> Int
  -- | Write the object to the specified memory address.
  poke :: Addr -> f m ->. Token
  -- | Read the object from the specified memory address. This is
  --   CPSed so that @m@ can be appropriately quantified. The utility
  --   function 'peek' is provided to recover the behavior one often
  --   wants.
  peek :: Addr -> Token ->. f m
  -- | Discard all linear components of the static object.
  forget :: f 'Static ->. Token
  -- | Combine the token with a reference inside of the object.
  inhume :: Token ->. f m ->. f m
  -- | Duplicate a token that exists in the object.
  exhume :: f m ->. (Token, f m)

-- peek :: forall (f :: Mode -> Type) (m :: Mode). Object f => Addr -> Token ->. (Token, f m)
-- peek a t = peeking @f @(Token,f m) a t (peekHelp @f @m)

-- peekHelp :: forall (f :: Mode -> Type) (n :: Mode). Token ->. (forall m. (f m)) ->. (Token,f n)
-- peekHelp t x = (t,x @n)

instance Object (Reference f) where
  size _ = PM.sizeOf (undefined :: Addr)
  {-# INLINE size #-}
  peek :: forall m. Addr -> Token ->. Reference f m
  peek x y = referencePeek' @f @m x y
  {-# INLINE peek #-}
  poke = unsafeCoerce referencePoke
  {-# INLINE poke #-}
  forget (Reference _ t0) = t0
  {-# INLINE forget #-}
  inhume a (Reference addr b) = Reference addr (a <>. b)
  {-# INLINE inhume #-}
  exhume (Reference addr a) = C.uncurry (\b c -> (b, Reference addr c)) (coappend a)
  {-# INLINE exhume #-}

-- | Recover an 'Object' instance from a 'Prim' instance.
--   This currently requires using @unsafeCoerce@ internally.
data PrimObject :: Type -> Mode -> Type where
  PrimObject :: a -> Token ->. PrimObject a m

-- instance C.Movable a => C.Movable (PrimObject a m) where
--   move (PrimObject a) = movePrimObjectHelp (C.move a)
-- 
-- movePrimObjectHelp :: C.Unrestricted a ->. C.Unrestricted (PrimObject a m)
-- movePrimObjectHelp (C.Unrestricted a') = C.Unrestricted (PrimObject a')

instance (Prim a) => Object (PrimObject a) where
  size _ = PM.sizeOf (undefined :: a)
  poke = unsafeCoerce (primPoke @a)
  peek :: forall m. Addr -> Token ->. PrimObject a m
  peek x y = (primPeek' @a x y)
  forget (PrimObject x t0) = t0
  inhume t0 (PrimObject a t1) = PrimObject a (t0 <>. t1)
  exhume (PrimObject a t0) = C.uncurry (\t1 t2 -> (t1,PrimObject a t2)) (C.coappend t0)

ununit :: () ->. a ->. a
ununit () a = a

{-# INLINE referencePeek' #-}
referencePeek' :: forall f m. Addr -> Token ->. Reference f m
referencePeek' = linearUnsafeCoerce (referencePeek @f)

{-# INLINE referencePeek #-}
referencePeek :: forall f m. Addr -> Token -> Reference f m
referencePeek (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> Reference a (Token s1)

{-# INLINE referencePoke #-}
referencePoke :: forall f m. Addr -> Reference f m -> Token
referencePoke (Addr addr) (Reference a (Token s0)) =
  case PM.writeOffAddr# addr 0# a s0 of
    s1 -> Token s1

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
primPoke :: forall a m. Prim a => Addr -> PrimObject a m -> Token
primPoke (Addr addr) (PrimObject a (Token s0)) = Token (PM.writeOffAddr# addr 0# a s0)

{-# INLINE primPeek' #-}
primPeek' :: forall a m. Prim a => Addr -> Token ->. PrimObject a m
primPeek' = linearUnsafeCoerce (primPeek @a)

{-# INLINE primPeek #-}
primPeek :: forall a m. Prim a => Addr -> Token -> PrimObject a m
primPeek (Addr addr) (Token s0) = case PM.readOffAddr# addr 0# s0 of
  (# s1, a #) -> PrimObject a (Token s1)

linearUnsafeCoerce :: a ->. b
linearUnsafeCoerce = unsafeCoerce unsafeCoerce

