{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}

module Linear.Identity
  ( Identity(..)
  , runIdentity
  ) where

import Data.Kind (Type)

newtype Identity :: Type -> Type where
  Identity :: a ->. Identity a

runIdentity :: Identity a ->. a
runIdentity (Identity a) = a

