{-# language LinearTypes #-}
{-# language TypeOperators #-}

module Example
  ( run
  ) where

import Data.Linear.Types
import Data.Semigroup.Linear (linearAppend)
import qualified Data.Linear.Struct as S
import qualified Data.Linear.Ref as R
import qualified Data.Linear.Deallocation as D

run :: IO ()
run = do
  putStrLn (show (R.result alpha))
  putStrLn (show (R.result beta))

alpha :: Result Int
alpha =
  S.withAllocation (55 :: Int) $. \n ->
  S.withAllocation (42 :: Int) $. \m ->
  finish (S.read (S.graft (R.deallocate (R.modify_ (R.allocate n) (\x -> S.write x (12 :: Int)))) (S.deallocate m)))

beta :: Result Int
beta =
  S.withAllocation (55 :: Int) $. \n ->
  S.withAllocation (12 :: Int) $. \m ->
  finish (S.read (linearAppend m n))

($.) :: (a ->. b) ->. a ->. b
f $. a = f a
 
finish :: (Struct a, Unrestricted a) ->. Result a
finish (x,y) = D.finish (S.deallocate x) y

instance Semigroup Int where
  (<>) = (+)

