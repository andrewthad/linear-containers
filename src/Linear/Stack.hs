{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}

module Linear.Stack
  ( Stack
  , empty
  , push
  , pop
    -- * Consume
  , foldl
  ) where

import Prelude hiding (foldl)

import Data.Kind (Type)
import Linear.Slate (Slate)
import Linear.Types (Mode(Static,Dynamic),Token,Object)
import Linear.Class (Unrestricted(..))

import qualified Linear.Slate as S
import qualified Linear.Class as C

-- | A stack backed by a contiguous array. The array doubles in
--   size whenever there is no more space. The array backing the
--   stack never shrinks.
data Stack :: (Mode -> Type) -> Mode -> Type where
  -- Contains a slate and the slate's maximum number of
  -- elements. We reallocate a new slate whenever we fill up.
  Stack :: {-# UNPACK #-} !(Slate f m) ->. {-# UNPACK #-} !Int -> Stack f m

empty :: Object f => Token ->. Stack f 'Dynamic
empty t = Stack (S.allocate 1 t) 1

push :: Object f => Stack f 'Dynamic ->. f 'Dynamic ->. Stack f 'Dynamic
push (Stack slate0 maxSz) val = C.uncurry
  (\slate1 (Unrestricted len) -> pushHelp (len < maxSz) slate1 val maxSz)
  (S.length slate0)

pushHelp :: Object f => Bool ->. Slate f 'Dynamic ->. f 'Dynamic ->. Int -> Stack f 'Dynamic
pushHelp True slate1 val maxSz = Stack (S.push slate1 val) maxSz
pushHelp False slate1 val maxSz = Stack (S.push (S.resize slate1 (maxSz * 2)) val) (maxSz * 2)

pop :: Object f => Stack f m ->. (Stack f m, f m)
pop (Stack slate0 maxSz) = C.first (\slate1 -> Stack slate1 maxSz) (S.pop slate0)

-- | Left fold over the slate, stack in the accumulator. This deallocates
--   the stack as it walks over it. This is an unusual way to interact with
--   a stack since it handles the oldest element first.
foldl :: Object f
  =>  (b ->. f 'Dynamic ->. b)
  ->  b
  ->. Stack f 'Dynamic
  ->. (Token,b)
foldl g b (Stack s _) = S.foldl g b s

