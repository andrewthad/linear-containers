{-# language BangPatterns #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.Array.Ref
  ( insert
  , singleton
  , modify
  ) where

import Data.Primitive (Addr)
import Data.Linear.Unsafe (RefArray(..),Ref(..))
import Foreign.Marshal.Alloc (mallocBytes)
import GHC.IO (IO(..))
import GHC.Exts (State#,RealWorld)
import Unsafe.Coerce (unsafeCoerce)
import Foreign.Storable (peekElemOff, pokeElemOff)
import qualified Data.Primitive as PM

-- | Insert a reference into an array at the given index. This does
-- not check to see if the insertion causes the array to overflow.
-- An overflown array leads to undefined behavior. 
insert ::
      RefArray a -- ^ array
  ->. Int -- ^ size of array
  ->  Ref a -- ^ reference
  ->. Int -- ^ index to insert at: @[0,sz]@
  ->  RefArray a
insert = unsafeCoerce insertNonlinear

insertNonlinear :: RefArray a -> Int -> Ref a -> Int -> RefArray a
insertNonlinear (RefArray arr s0) sz (Ref ptr s1) ix = error "insertNonlinear: write me"

modify ::
      RefArray a -- ^ array
  ->. Int -- ^ index into array
  ->  (Ref a ->. (Ref a,b)) -- ^ first tuple element is saved at the index, second is returned
  ->. (RefArray a, b)
modify = unsafeCoerce modifyNonlinear

modifyNonlinear :: RefArray a -> Int -> (Ref a ->. (Ref a,b)) -> (RefArray a, b)
modifyNonlinear (RefArray arr s0) ix f = case unIO (peekElemOff arr ix) s0 of
  (# s1, ptr0 #) -> case f (Ref ptr0 s1) of
    -- Is this sound? If b is a reference type, it definitely is. If not,
    -- I'm still pretty confident that it is.
    (Ref ptr1 s2,b) -> case unIO (pokeElemOff arr ix ptr1) s2 of
      (# s3, _ #) -> (RefArray arr s3, b)

-- | Create an array with a single element at index 0. Passing in
-- an maximum size less than 1 results in undefined behavior. 
singleton ::
      Int -- ^ maximum size of array
  ->  Ref a -- ^ reference
  ->. RefArray a
singleton = unsafeCoerce singletonNonlinear

singletonNonlinear :: Int -> Ref a -> RefArray a
singletonNonlinear n (Ref ptr s0) =
  case unIO (mallocBytes (n * PM.sizeOf (undefined :: Addr))) s0 of
    (# s1, arr #) -> RefArray arr s1

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

