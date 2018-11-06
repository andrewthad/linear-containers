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
  , Action(..)
  , withAllocatedBytes
  , withDeallocate
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Exts (Ptr(..),State#,RealWorld,realWorld#,forkState#,joinState#)
import GHC.IO (IO(..))
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)
import Data.Primitive (Addr(..))
import Foreign.Marshal.Alloc (mallocBytes,free)
import qualified Linear.Class as L

data Mode = Dynamic | Static

data Reference :: Type -> Type where
  Reference :: {-# UNPACK #-} !Addr -> {-# UNPACK #-} !Token ->. Reference a

-- | Tokens are used in order to enforce a sequencing of operations
--   when there is no suitable data to attach a state token to. This
--   is aesthetically undesirable, but it is sometimes necessary.
data Token where
  Token :: !(State# RealWorld) ->. Token

data Action :: Mode -> Type where
  Action :: !(State# RealWorld) ->. Action m

instance L.Semigroup Token where
  append (Token a) (Token b) = Token (joinState# a b)

instance L.Cosemigroup Token where
  coappend (Token a) = forkStateHelper (forkState# a)

-- instance L.Semigroup (Action m) where
--   append (Action a) (Action b) = Action (joinState# a b)
-- 
-- instance L.Cosemigroup (Action m) where
--   coappend (Action a) = forkStateActionHelper (forkState# a)

withDeallocate :: Addr -> Action 'Dynamic ->. (Action 'Dynamic ->. a) ->. a
withDeallocate = unsafeCoerce withDeallocateNonlinear

withDeallocateNonlinear :: Addr -> Action 'Dynamic -> (Action 'Dynamic -> a) -> a
withDeallocateNonlinear (Addr addr#) (Action s0) f = case unIO (free (Ptr addr#)) s0 of
  (# s1, _ #) -> f (Action s1)

-- | This function is not safe to use in general since it does not
-- actually enforce that the token is only used once.
withAllocatedBytes :: Int -> Action 'Dynamic ->. (Addr -> Action 'Dynamic ->. a) ->. a
withAllocatedBytes = unsafeCoerce withAllocatedBytesNonlinear

withAllocatedBytesNonlinear :: Int -> Action 'Dynamic -> (Addr -> Action 'Dynamic ->. a) ->. a
withAllocatedBytesNonlinear n (Action s0) f = case unIO (mallocBytes n) s0 of
  (# s1, Ptr addr# #) -> f (Addr addr#) (Action s1)

forkStateHelper :: (# State# RealWorld, State# RealWorld #) ->. (Token, Token)
forkStateHelper (# s0, s1 #) = (Token s0, Token s1)

forkStateActionHelper :: (# State# RealWorld, State# RealWorld #) ->. (Action m, Action m)
forkStateActionHelper (# s0, s1 #) = (Action s0, Action s1)

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

