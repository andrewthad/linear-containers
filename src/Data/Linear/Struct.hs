{-# language BangPatterns #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.Struct
  ( withAllocation
  , deallocate
  , read
  , write
  , graft
  ) where

import Prelude hiding (read)

import GHC.Ptr (Ptr(..))
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (free,mallocBytes)
import Data.Linear.Unsafe (Buffer(..),Struct(..), Deallocation(..), Result(..))
import Data.Linear.Types (Unrestricted(..))
import Data.Primitive (Prim)
import GHC.IO (IO(..))
import GHC.Exts (RealWorld,State#,runRW#,realWorld#)
import Unsafe.Coerce (unsafeCoerce)
import Data.Proxy (Proxy(..))

import qualified Data.Primitive as PM
import qualified Data.Primitive.Ptr as PM

bufferToStruct :: Buffer ->. Struct a
bufferToStruct (Buffer ptr s) = Struct (castPtr ptr) s

withAllocation :: forall a b. Prim a => a ->. (Struct a ->. Result b) ->. Result b
withAllocation = unsafeCoerce (withAllocationNonlinear @a @b)

withAllocationNonlinear :: forall a b. Prim a => a -> (Struct a ->. Result b) -> Result b
withAllocationNonlinear a f = case mallocBytes (PM.sizeOf (undefined :: a)) of
  IO g -> case runRW# g of
    (# s, Ptr addr #) -> case PM.writeOffAddr# addr 0# a s of
      s1 -> f (Struct (Ptr addr) s)

deallocate :: Struct a ->. Deallocation
deallocate = unsafeCoerce deallocateNonlinear

deallocateNonlinear :: Struct a -> Deallocation
deallocateNonlinear (Struct ptr0 s0) = case unIO (free ptr0) s0 of
  (# s1, _ #) -> Deallocation s1

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

read :: forall a. Prim a => Struct a ->. (Struct a, Unrestricted a)
read = unsafeCoerce (readNonlinear @a)

readNonlinear :: forall a. Prim a => Struct a -> (Struct a, Unrestricted a)
readNonlinear (Struct ptr s0) = case unIO (PM.readOffPtr ptr 0) s0 of
  (# s1, a #) -> (Struct ptr s1, Unrestricted a)

write :: forall a. Prim a => Struct a ->. a -> Struct a
write = unsafeCoerce (writeNonlinear @a)

writeNonlinear :: forall a. Prim a => Struct a -> a -> Struct a
writeNonlinear (Struct ptr s0) a = case unIO (PM.writeOffPtr ptr 0 a) s0 of
  (# s1, _ #) -> Struct ptr s1

graft :: Struct a ->. Deallocation ->. Struct a
graft (Struct ptr s0) (Deallocation s1) = Struct ptr (jamState# s0 s1)

{-# NOINLINE jamState# #-}
jamState# :: State# RealWorld ->. State# RealWorld ->. State# RealWorld
jamState# = unsafeCoerce jamStateNonlinear#

jamStateNonlinear# :: State# RealWorld -> State# RealWorld -> State# RealWorld
jamStateNonlinear# !_ !_ = realWorld#

