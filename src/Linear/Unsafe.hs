{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Linear.Unsafe
  ( Mode(..)
  , Reference(..)
  , Token(..)
  , withAllocatedBytes
  , withDeallocate
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Exts (Ptr(..),State#,RealWorld,realWorld#)
import GHC.IO (IO(..))
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)
import Data.Primitive (Addr(..))
import Foreign.Marshal.Alloc (mallocBytes,free)
import qualified Linear.Class as L

data Mode = Dynamic | Static

data Reference :: (Mode -> Type) -> Mode -> Type where
  Reference :: {-# UNPACK #-} !Addr -> {-# UNPACK #-} !Token ->. Reference f m

-- | Tokens are used in order to enforce a sequencing of operations
--   when there is no suitable data to attach a state token to. This
--   is aesthetically undesirable, but it is sometimes necessary.
data Token where
  Token :: !(State# RealWorld) ->. Token

instance L.Semigroup Token where
  append (Token a) (Token b) = Token (jamState# a b)

instance L.Cosemigroup Token where
  coappend (Token a) = dupState# a

withDeallocate :: Addr -> Token ->. (Token ->. a) ->. a
withDeallocate = unsafeCoerce withDeallocateNonlinear

withDeallocateNonlinear :: Addr -> Token -> (Token -> a) -> a
withDeallocateNonlinear (Addr addr#) (Token s0) f = case unIO (free (Ptr addr#)) s0 of
  (# s1, _ #) -> f (Token s1)

-- | This function is not safe to use in general since it does not
-- actually enforce that the token is only used once.
withAllocatedBytes :: Int -> Token ->. (Addr -> Token ->. a) ->. a
withAllocatedBytes = unsafeCoerce withAllocatedBytesNonlinear

withAllocatedBytesNonlinear :: Int -> Token -> (Addr -> Token ->. a) ->. a
withAllocatedBytesNonlinear n (Token s0) f = case unIO (mallocBytes n) s0 of
  (# s1, Ptr addr# #) -> f (Addr addr#) (Token s1)

jamState# :: State# RealWorld ->. State# RealWorld ->. State# RealWorld
jamState# = unsafeCoerce jamStateNonlinear#

{-# NOINLINE jamStateNonlinear# #-}
jamStateNonlinear# :: State# RealWorld -> State# RealWorld -> State# RealWorld
jamStateNonlinear# !_ !_ = realWorld#

{-# NOINLINE dupState# #-}
dupState# :: State# RealWorld ->. (Token, Token)
dupState# = unsafeCoerce dupStateNonlinear#

dupStateNonlinear# :: State# RealWorld -> (Token, Token)
dupStateNonlinear# s = (Token s, Token s)

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

