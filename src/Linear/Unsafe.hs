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
  ( Reference(..)
  , Token(..)
  , Static(..)
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

data Reference :: Type -> Type where
  Reference :: {-# UNPACK #-} !Addr -> {-# UNPACK #-} !Token ->. Reference a

-- | Tokens are used in order to enforce a sequencing of operations
--   when there is no suitable data to attach a state token to. This
--   is aesthetically undesirable, but it is sometimes necessary.
data Token where
  Token :: !(State# RealWorld) ->. Token

newtype Static :: Type -> Type where
  Static :: a ->. Static a

instance L.Functor Static where
  map f (Static x) = Static (f x)

instance L.Unzip Static where
  unzip (Static (x,y)) = (Static x, Static y)

instance L.Semidecisive Static where
  decide (Static (Left a)) = Left (Static a)
  decide (Static (Right b)) = Right (Static b)

instance L.Semigroup Token where
  append (Token a) (Token b) = Token (joinState# a b)

instance L.Cosemigroup Token where
  coappend (Token a) = forkStateHelper (forkState# a)

-- instance L.Semigroup (Action m) where
--   append (Action a) (Action b) = Action (joinState# a b)
-- 
-- instance L.Cosemigroup (Action m) where
--   coappend (Action a) = forkStateActionHelper (forkState# a)

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

forkStateHelper :: (# State# RealWorld, State# RealWorld #) ->. (Token, Token)
forkStateHelper (# s0, s1 #) = (Token s0, Token s1)

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

