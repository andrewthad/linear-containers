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
import Linear.Unsafe (Reference(..),Mode(..),Token(..),Slate(..))
import Data.Primitive (Addr(..),Prim)
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (RealWorld,State#,Addr#)

import qualified GHC.Exts as E
import qualified Data.Primitive as PM
import qualified Linear.Class as C
import qualified Linear.Unsafe as U

-- | Object types can be serialized to the unmanaged heap. This includes
--   types containing a 'Reference' but excludes types containing functions.
--   Additionally, the object must transitively contain at least one token
--   or reference. This means that that we can inject a sequence token into
--   the data and enforce a particular ordering of operations.
class Object (f :: Mode -> Type) where 
  -- | The size of the object in bytes.
  size :: Proxy f -> Int
  -- | Write the object to the specified memory address. This should
  --   force all pending peeks to happen before it attempts to
  --   write anything.
  poke :: Addr -> f m ->. Token
  -- | Read the object from the specified memory address.
  peek :: Addr -> Token ->. f m
  -- | Discard all linear components of the static object.
  forget :: f 'Static ->. Token
  -- | Combine the token with a reference inside of the object.
  inhume :: Token ->. f m ->. f m
  -- | Duplicate a token that exists in the object. We currently
  --   do not require (but we should) that the token given by
  --   exhume is one that guarantees that all peeks have happened.
  --   If we do start to require this, we might be able to get
  --   rid of @exhumeAll@.
  exhume :: f m ->. (Token, f m)
  -- | Merge all tokens that exists in the object and duplicate
  --   that result. The linearity checker will not help ensure
  --   that this function has a correct implementation, so be
  --   sure that every field containing one or more tokens has
  --   all of its tokens duplicated. This function is crucial
  --   for dealloctations, where all reads must happen prior
  --   to freeing the object.
  exhumeAll :: f m ->. (Token, f m)

class Object f => Relocatable f where
  -- | Destroy an object. This follows all references recursively,
  --   deleting from memory anything reachable from the root.
  destroy :: f 'Dynamic ->. Token
  -- | Make a deep copy on an object.
  clone :: f 'Dynamic ->. (f 'Dynamic, f 'Dynamic)

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
  exhumeAll (Reference addr a) = C.uncurry (\b c -> (b, Reference addr c)) (coappend a)
  {-# INLINE exhumeAll #-}

instance Relocatable f => Relocatable (Reference f) where
   destroy (Reference addr t0) = C.uncurry
    (\t1 a -> U.withDeallocate addr t1 (\t2 -> destroy (inhume t2 a)))
    (exhume (peek addr t0 :: f 'Dynamic))
   clone (Reference addr t0) = C.first
    (Reference addr)
    (exhume (allocateInternal (peek addr t0)))

instance Object (Slate f) where
  size _ = 2 * PM.sizeOf (undefined :: Addr)
  {-# INLINE size #-}
  peek :: forall m. Addr -> Token ->. Slate f m
  peek addr t0 = step1 (primPeek' addr t0)
    where
    step1 :: PrimObject Addr m ->. Slate f m
    step1 (PrimObject resAddr t1) = step2 resAddr (primPeek' (PM.plusAddr addr (PM.sizeOf (undefined :: Addr))) t1)
    step2 :: Addr -> PrimObject Int m ->. Slate f m
    step2 resAddr (PrimObject resInt t2) = Slate resAddr resInt t2
  {-# INLINE peek #-}
  poke = unsafeCoerce slatePoke
  {-# INLINE poke #-}
  forget (Slate _ _ t0) = t0
  {-# INLINE forget #-}
  inhume a (Slate addr sz b) = Slate addr sz (a <>. b)
  {-# INLINE inhume #-}
  exhume (Slate addr sz a) = C.uncurry (\b c -> (b, Slate addr sz c)) (coappend a)
  {-# INLINE exhume #-}
  exhumeAll (Slate addr sz a) = C.uncurry (\b c -> (b, Slate addr sz c)) (coappend a)
  {-# INLINE exhumeAll #-}

-- This is a copy of the allocate function from Linear.Reference.
-- It is copied to prevent cyclic modules.
allocateInternal :: forall f. Object f
  =>  f 'Dynamic
  ->. Reference f 'Dynamic
allocateInternal a = C.uncurry
  (\t0 a' ->
    U.withAllocatedBytes (size (Proxy :: Proxy f)) t0
    (\addr t1 -> Reference addr (t1 <>. poke addr a'))
  ) (exhume a)


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
  exhumeAll (PrimObject a t0) = C.uncurry (\t1 t2 -> (t1,PrimObject a t2)) (C.coappend t0)

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

{-# INLINE slatePoke #-}
slatePoke :: forall f m. Addr -> Slate f m -> Token
slatePoke (Addr addr) (Slate a sz (Token s0)) =
  case PM.writeOffAddr# addr 0# a s0 of
    s1 -> case PM.writeOffAddr# addr (PM.sizeOf# (undefined :: Addr)) sz s1 of
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

