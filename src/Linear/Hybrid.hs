{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language LinearTypes #-}

module Linear.Hybrid
  ( Functor(..)
  , Lens
  , Lens'
  ) where

import Prelude ()
import Linear.Identity (Identity(..),runIdentity)

class Functor f where
  map :: (a ->. b) -> f a ->. f b

instance Functor [] where
  map f [] = []
  map f (x : xs) = f x : (map f xs)

type Lens s t a b = forall f. Functor f => (a ->. f b) -> s ->. f t
type Lens' s a = Lens s s a a

type Setter s t a b = (a ->. Identity b) -> s ->. Identity t

over :: Setter s t a b -> (a ->. b) -> s ->. t
over l f y = runIdentity (l (\x -> Identity (f x)) y)



