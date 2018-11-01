{-# language LinearTypes #-}

module Data.Linear.Class
  ( Applicative(..)
  , Semiapplicative(..)
  , Functor(..)
  , (<*>.)
  ) where

import Prelude ()

class Functor f where
  map :: (a ->. b) ->. f a ->. f b

class Semiapplicative f where
  apply :: f (a ->. b) ->. f a ->. f b

class Applicative f where
  pure :: a ->. f a

infixl 4 <*>.
(<*>.) :: Semiapplicative f => f (a ->. b) ->. f a ->. f b
(<*>.) = apply
