{-# language BangPatterns #-}
{-# language GADTSyntax #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Linear.Ref
  ( allocate
  , withAllocation
  , deallocate
  , deallocateExplicit
  , result
  , modify
  , modify_
  , read
  ) where

import Prelude hiding (read)

import Data.Proxy (Proxy(..))
import Data.Word
import Data.Linear.Unsafe (Ref(..),Buffer(..),Result(..),Deallocation(..))
import Data.Linear.Types (Indirection(..),Unrestricted(..),Heap(..))
import Foreign.Marshal.Alloc (mallocBytes,free)
import Foreign.Ptr (Ptr,castPtr)
import GHC.Exts
import GHC.IO (IO(..))
import GHC.Ptr (Ptr(..))
import Unsafe.Coerce (unsafeCoerce)

-- writeStruct :: Struct a ->. a ->. Struct a
-- writeStruct (Struct ptr s0) a = bufferToStruct (writeIndirection (Buffer (castPtr ptr) s0) a)

-- Think about some kind of interface for a reader token. This would allow
-- tail recursive reads to work correctly:
--
--   readToken :: Ref a ->. (a ->. ReadToken ->. (ReadToken,Unrestricted b)) -> Ref a
--   stepRead :: Ref a ->. ReadToken ->. (ReadToken,a)
--   discardRead :: Ref a ->. ReadToken ->. ReadToken
--
-- This would allow the user to destroy references with deallocating. But,
-- I am not sure if it is safe. The idea is that readToken would be used
-- to establish a context. The claim it makes is that only its children
-- can be read in the callback (Is this claim true?). The final ReadToken
-- that we get back at the end has a State# token inside of it, and we
-- graft that into the top-level Ref. Now, none of the children would get
-- updated with this newer state token, but that may not matter since
-- you have to read the parent to access any of the children. This sequences
-- the actions such that any future writes must happen after all of the
-- reads. Think about this more.

read :: Indirection a => Ref a ->. (a ->. Unrestricted b) ->. (Ref a, Unrestricted b)
read = error "Uheotnutoh"

modify :: Indirection a => Ref a ->. (a ->. (a,b)) ->. (Ref a,b)
modify (Ref ptr s0) = modifyStep1 (readIndirection 0 (Buffer (castPtr ptr) s0))

modifyStep1 :: Indirection a => (Buffer, a) ->. (a ->. (a,b)) ->. (Ref a, b)
modifyStep1 (buf, a) f = modifyStep2 buf (f a)

modifyStep2 :: Indirection a => Buffer ->. (a,b) ->. (Ref a,b)
modifyStep2 buf (a,b) = (bufferToRef (writeIndirection 0 buf a),b)

modify_ :: Indirection a => Ref a ->. (a ->. a) ->. Ref a
modify_ (Ref ptr s0) = modifyStep_ (readIndirection 0 (Buffer (castPtr ptr) s0))

modifyStep_ :: Indirection a => (Buffer, a) ->. (a ->. a) ->. Ref a
modifyStep_ (buf, a) f = bufferToRef (writeIndirection 0 buf (f a))

bufferToRef :: Buffer ->. Ref a
bufferToRef (Buffer ptr s) = Ref (castPtr ptr) s

result :: Result a ->. Unrestricted a
result = unsafeCoerce resultNonlinear

resultNonlinear :: Result a -> Unrestricted a
resultNonlinear (Result a0 !s0) = case seq# a0 s0 of
  -- This is the use of a state token in this library that
  -- I do not feel totally confident of.
  (# _, a1 #) -> Unrestricted a1

-- | This deallocates the area in the unmanaged heap. However, it first reads
--   the value and then returns that. The user will then need to deallocate
--   any additional pointers that live in this object.
deallocate :: forall a. Indirection a => Ref a ->. a
deallocate = unsafeCoerce (deallocateNonlinear @a)

deallocateNonlinear :: Indirection a => Ref a -> a
deallocateNonlinear (Ref ptr0 s0) = case readIndirection 0 (Buffer (castPtr ptr0) s0) of
  (Buffer ptr1 s1, a) -> case unIO (free ptr1) s1 of
    (# s2, _ #) -> patchIndirection (Deallocation s2) a

deallocateExplicit :: forall a. Heap a => Ref a ->. (Deallocation,a)
deallocateExplicit = unsafeCoerce (deallocateExplicitNonlinear @a)

deallocateExplicitNonlinear :: Heap a => Ref a -> (Deallocation,a)
deallocateExplicitNonlinear (Ref ptr0 s0) = case readIndirection 0 (Buffer (castPtr ptr0) s0) of
  (Buffer ptr1 s1, a) -> case unIO (free ptr1) s1 of
    (# s2, _ #) -> (Deallocation s2, a)
  
unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

-- | Allocate a reference. This is sound because the @Indirection@ typeclass
--   gives us a guarantee that a value of type @a@ must contain a @Ref@ somewhere
--   inside of it. Consequently, it is impossible to conjure up a value of type @a@
--   outside of a linearly-scoped computation.
allocate :: forall a. Heap a => a ->. Ref a
allocate = unsafeCoerce (allocateNonlinear @a)

allocateNonlinear :: forall a. Heap a => a -> Ref a
allocateNonlinear a = case mallocBytes (sizeIndirection (Proxy :: Proxy a)) of
  IO g -> case runRW# g of
    (# s, ptr #) -> case writeIndirection 0 (Buffer ptr s) a of
      Buffer ptr1 s1 -> Ref (castPtr ptr1) s1

-- | Allocate a reference and use it to build a @Result@. The reference is not
--   deallocated by this function. However, the user must deallocate it in order
--   to construct a @Result@. This is enforced by the type checker.
withAllocation :: forall a b. Indirection a => a ->. (Ref a ->. Result b) ->. Result b
withAllocation = unsafeCoerce (withAllocationNonlinear @a @b)

withAllocationNonlinear :: forall a b. Indirection a => a -> (Ref a ->. Result b) -> Result b
withAllocationNonlinear a f = case mallocBytes (sizeIndirection (Proxy :: Proxy a)) of
  IO g -> case runRW# g of
    (# s, ptr #) -> case writeIndirection 0 (Buffer ptr s) a of
      Buffer ptr1 s1 -> f (Ref (castPtr ptr1) s1)

{-# NOINLINE dupState# #-}
dupState# :: State# RealWorld ->. (# State# RealWorld, State# RealWorld #)
dupState# = unsafeCoerce dupStateNonlinear#

dupStateNonlinear# :: State# RealWorld -> (# State# RealWorld, State# RealWorld #)
dupStateNonlinear# s = (# s, s #)

-- dupAddr# :: Addr# ->. (Addr# ->. Addr# ->. a) ->. a
-- dupAddr# = unsafeCoerce (\x f -> f x x)

