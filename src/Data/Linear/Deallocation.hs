{-# language BangPatterns #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.Deallocation
  ( finish
  , graft
  ) where

import GHC.Exts (State#,RealWorld,realWorld#)
import Data.Linear.Types (Unrestricted(..))
import Data.Linear.Unsafe (Deallocation(..),Result(..))
import Unsafe.Coerce (unsafeCoerce)

finish :: Deallocation ->. Unrestricted a ->. Result a
finish (Deallocation s) (Unrestricted a) = Result a s

graft :: Deallocation ->. Deallocation ->. Deallocation
graft (Deallocation s1) (Deallocation s2) = Deallocation (jamState# s1 s2)

{-# NOINLINE jamState# #-}
jamState# :: State# RealWorld ->. State# RealWorld ->. State# RealWorld
jamState# = unsafeCoerce jamStateNonlinear#

jamStateNonlinear# :: State# RealWorld -> State# RealWorld -> State# RealWorld
jamStateNonlinear# !_ !_ = realWorld#

