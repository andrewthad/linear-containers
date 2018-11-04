{-# language GADTSyntax #-}
{-# language LinearTypes #-}

module Linear.State
  ( State(..)
  , state
  , runState
  ) where

import qualified Linear.Class as L

newtype State s a where
  State :: (s ->. (s,a)) ->. State s a

runState :: State s a ->. s ->. (s,a)
runState (State f) s = f s

state :: (s ->. (s,a)) ->. State s a
state = State

instance L.Functor (State s) where
  map f (State g) = State ( \s0 -> mapHelper f (g s0))

instance L.Semiapplicative (State s) where
  apply (State f) (State g) = State (\s0 -> applyHelper f (g s0))

instance L.Applicative (State s) where
  pure a = State (\s -> (s,a))

instance L.Monad (State s) where
  bind (State f) g = State $. \s -> bindHelper (f s) g

bindHelper :: (s,a) ->. (a ->. State s b) ->. (s,b)
bindHelper (s,a) f = runState (f a) s

mapHelper :: (a ->. b) ->. (s,a) ->. (s,b)
mapHelper f (s,a) = (s, f a)

applyHelper :: (s ->. (s,a ->. b)) ->. (s,a) ->. (s,b)
applyHelper f (s,a) = applyHelper2 (f s) a

applyHelper2 :: (s,a ->. b) ->. a ->. (s,b)
applyHelper2 (s,f) a = (s, f a)

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a
