{-# language BangPatterns #-}
{-# language GADTSyntax #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.Unsafe
  ( Struct(..)
  , Ref(..)
  , Buffer(..)
  , Result(..)
  , Deallocation(..)
  , RefArray(..)
  , StructArray(..)
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Exts (State#,RealWorld)

data Struct a where
  Struct :: !(Ptr a) -> !(State# RealWorld) ->. Struct a
data Ref a where
  Ref :: !(Ptr a) -> !(State# RealWorld) ->. Ref a
data Buffer where
  Buffer :: !(Ptr Word8) -> !(State# RealWorld) ->. Buffer
data Result a where
  Result :: a -> !(State# RealWorld) ->. Result a
data Deallocation where
  Deallocation :: !(State# RealWorld) ->. Deallocation

-- | These arrays are lists with a maximum size. They do not keep track
-- of the maximum size or of the current number of stored elements. This
-- must be tracked externally. Accessing an element at an index that
-- exceeds the current size results in undefined behavior.
data RefArray a where
  RefArray :: !(Ptr (Ptr a)) -> !(State# RealWorld) ->. RefArray a

data StructArray a where
  StructArray :: !(Ptr a) -> !(State# RealWorld) ->. StructArray a
