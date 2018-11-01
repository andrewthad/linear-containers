{-# language BangPatterns #-}
{-# language GADTSyntax #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.Types
  ( U.Struct
  , U.Ref
  , U.Buffer
  , U.Result
  , U.Deallocation
  , U.RefArray
  , Unrestricted(..)
  , Heap(..)
  , Indirection(..)
  ) where

import Data.Proxy (Proxy(..))
import GHC.IO (IO(..))
import GHC.Ptr (Ptr(..))
import GHC.Exts (Addr#,State#,Int#,RealWorld,Int(I#),realWorld#,writeAddrOffAddr#,readAddrOffAddr#)
import Unsafe.Coerce (unsafeCoerce)
import Data.Primitive (Addr,Prim)
import Foreign.Ptr (castPtr)
import qualified Data.Primitive as PM
import qualified Data.Primitive.Ptr as PM
import qualified Data.Linear.Unsafe as U

data Unrestricted a where
  Unrestricted :: a -> Unrestricted a
  deriving (Show)

class Heap a where
  sizeIndirection :: Proxy a -> Int
  writeIndirection :: Int -> U.Buffer ->. a ->. U.Buffer
  -- ^ offset for write given in elements of type a
  readIndirection :: Int -> U.Buffer ->. (U.Buffer, a)
  -- ^ offset for read given in elements of type a

-- | A class for data types that contain pointers to objects on the
--   unmanaged heap.
class Heap a => Indirection a where
  patchIndirection :: U.Deallocation ->. a ->. a

instance Heap Int where
  sizeIndirection _ = PM.sizeOf (undefined :: Int)
  writeIndirection = unsafeCoerce (writePrimNonlinear @Int)
  readIndirection = unsafeCoerce (readPrimNonlinear @Int)

readPrimNonlinear :: Prim a => Int -> U.Buffer -> (U.Buffer,a)
readPrimNonlinear ix (U.Buffer dst s0) = case unIO (PM.readOffPtr (castPtr dst) ix) s0 of
  (# s1, a #) -> (U.Buffer dst s1,a)

writePrimNonlinear :: Prim a => Int -> U.Buffer -> a -> U.Buffer
writePrimNonlinear ix (U.Buffer dst s0) a = case unIO (PM.writeOffPtr (castPtr dst) ix a) s0 of
  (# s1, _ #) -> U.Buffer dst s1

instance Heap (U.Ref a) where
  sizeIndirection _ = PM.sizeOf (undefined :: Addr)
  readIndirection = unsafeCoerce readIndirectionRefNonlinear
  writeIndirection (I# off) (U.Buffer (Ptr dst) s0) (U.Ref (Ptr e) s1) = U.Buffer (Ptr dst) (linearWriteAddrOffAddr# dst off e (jamState# s0 s1))

instance Indirection (U.Ref a) where
  patchIndirection (U.Deallocation s0) (U.Ref dst s1) = U.Ref dst (jamState# s0 s1)

instance Heap (U.Struct a) where
  sizeIndirection _ = PM.sizeOf (undefined :: Addr)
  readIndirection = unsafeCoerce readIndirectionStructNonlinear
  writeIndirection (I# off) (U.Buffer (Ptr dst) s0) (U.Struct (Ptr e) s1) = U.Buffer (Ptr dst) (linearWriteAddrOffAddr# dst off e (jamState# s0 s1))

instance Indirection (U.Struct a) where
  patchIndirection (U.Deallocation s0) (U.Struct dst s1) = U.Struct dst (jamState# s0 s1)

{-# NOINLINE jamState# #-}
jamState# :: State# RealWorld ->. State# RealWorld ->. State# RealWorld
jamState# = unsafeCoerce jamStateNonlinear#

jamStateNonlinear# :: State# RealWorld -> State# RealWorld -> State# RealWorld
jamStateNonlinear# !_ !_ = realWorld#

linearWriteAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s ->. State# s
linearWriteAddrOffAddr# = unsafeCoerce writeAddrOffAddr#

readIndirectionRefNonlinear :: Int -> U.Buffer -> (U.Buffer, U.Ref a)
readIndirectionRefNonlinear (I# off) (U.Buffer (Ptr dst) s0) = case readAddrOffAddr# dst off s0 of
  (# s1, r #) -> (U.Buffer (Ptr dst) s1, U.Ref (Ptr r) s1)

readIndirectionStructNonlinear :: Int -> U.Buffer -> (U.Buffer, U.Struct a)
readIndirectionStructNonlinear (I# off) (U.Buffer (Ptr dst) s0) = case readAddrOffAddr# dst off s0 of
  (# s1, r #) -> (U.Buffer (Ptr dst) s1, U.Struct (Ptr r) s1)

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

