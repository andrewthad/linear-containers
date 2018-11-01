{-# language BangPatterns #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.Array.Struct
  ( insert
  , read
  , split
  ) where

import Prelude hiding (read)

import Data.Primitive (Addr)
import Data.Linear.Unsafe (StructArray(..),Ref(..))
import Foreign.Marshal.Alloc (mallocBytes)
import GHC.IO (IO(..))
import GHC.Exts (State#,RealWorld)
import Unsafe.Coerce (unsafeCoerce)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Data.Primitive (Prim)
import qualified Data.Primitive as PM
import qualified Data.Primitive.Ptr as PM

-- | Insert a reference into an array at the given index. This does
-- not check to see if the insertion causes the array to overflow.
-- An overflown array leads to undefined behavior. 
insert :: forall a.
      StructArray a -- ^ array
  ->. Int -- ^ size of array
  ->  a -- ^ value
  ->  Int -- ^ index to insert at: @[0,sz]@
  ->  StructArray a
insert = unsafeCoerce (insertNonlinear @a)

insertNonlinear :: StructArray a -> Int -> a -> Int -> StructArray a
insertNonlinear (StructArray arr s0) sz a ix = error "insertNonlinear: write me"

read :: forall a.
      StructArray a -- ^ array
  ->. Int -- ^ index into array
  ->  (StructArray a, a)
read = unsafeCoerce (readNonlinear @a)

readNonlinear :: StructArray a -> Int -> (StructArray a, a)
readNonlinear (StructArray arr s0) ix = error "huotenhu"

split :: forall a. Prim a
  =>  StructArray a -- ^ array
  ->. Int -- ^ maximum size of the array
  ->  Int -- ^ size of the array
  ->  Int -- ^ index into array: @[0,sz]@
  ->  (StructArray a, StructArray a)
split = unsafeCoerce (splitNonlinear @a)

splitNonlinear :: forall a. Prim a => StructArray a -> Int -> Int -> Int -> (StructArray a, StructArray a)
splitNonlinear (StructArray arr1 s0) maxSz sz ix =
  case unIO r s0 of
    (# s1, arr2 #) -> (StructArray arr1 s1, StructArray arr2 s1)
  where
  r = do
    arr2 <- mallocBytes (maxSz * (PM.sizeOf (undefined :: a)))
    PM.copyPtr arr2 (PM.advancePtr arr1 ix) (sz - ix)
    return arr2
  

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f



