{-# language LinearTypes #-}

module Data.Linear.State
  ( State(..)
  , state
  ) where

import qualified Data.Linear.Class as L

newtype State s a = State { runState :: s ->. (s,a) }

state :: (s ->. (s,a)) ->. State s a
state = State

instance L.Functor (State s) where
  map f (State g) = State ( \s0 -> mapHelper f (g s0))

instance L.Semiapplicative (State s) where
  apply (State f) (State g) = State (\s0 -> applyHelper f (g s0))

instance L.Applicative (State s) where
  pure a = State (\s -> (s,a))

mapHelper :: (a ->. b) ->. (s,a) ->. (s,b)
mapHelper f (s,a) = (s, f a)

applyHelper :: (s ->. (s,a ->. b)) ->. (s,a) ->. (s,b)
applyHelper f (s,a) = applyHelper2 (f s) a

applyHelper2 :: (s,a ->. b) ->. a ->. (s,b)
applyHelper2 (s,f) a = (s, f a)

