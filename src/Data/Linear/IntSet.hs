{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.IntSet
  ( Set
  ) where

import Data.Linear.Types (Ref,Indirection(..),Heap(..),Buffer,Deallocation)
import Data.Primitive (Addr)
import Unsafe.Coerce (unsafeCoerce)
import Data.Linear.State (state,runState)
import Data.Linear.Class ((<*>.))
import GHC.Exts (Int(I#))
import qualified Data.Linear.Class as L
import qualified Data.Linear.Ref as R
import qualified Data.Primitive as PM

infixr 9 $.

data Set where
  Set :: T n -> Set

data Nat = Z | S Nat

data N n where
  T1 :: Ref (T n) ->. !Int -> Ref (T n) ->. N n
  T2 :: Ref (T n) ->. !Int -> Ref (T n) ->. !Int -> Ref (T n) ->. N n

data T n where
  BR :: N n ->. T (S n)
  LF :: T Z

-- This read function in this instance is unsound. But
-- the way the everything in this module uses it, it ends
-- up being sound.
instance Heap (T n) where
  sizeIndirection _ = (PM.sizeOf (undefined :: Addr) * 6)
  writeIndirection off buf LF =
    writeIndirection 0 buf (0 :: Int)
  writeIndirection off buf (BR (T1 a b c)) =
       customWrite 3 c
    $. customWrite 2 b
    $. customWrite 1 a
    $. customWrite 0 (1 :: Int) buf
  writeIndirection off buf (BR (T2 a b c d e)) =
       customWrite 5 e
    $. customWrite 4 d
    $. customWrite 3 c
    $. customWrite 2 b
    $. customWrite 1 a
    $. customWrite 0 (2 :: Int) buf
  readIndirection off buf0 = readHelper (readIndirection 0 buf0)

readHelper :: (Buffer,Int) ->. (Buffer,T n)
readHelper (buf,0) = (buf,unsafeCoerce LF)
readHelper (buf,1) = readT1' buf
readHelper (buf,2) = readT2' buf

readT2' :: Buffer ->. (Buffer, T n)
readT2' = unsafeCoerce readT2

readT1' :: Buffer ->. (Buffer, T n)
readT1' = unsafeCoerce readT1

readT1 :: Buffer ->. (Buffer, T (S n))
readT1 buf = runState
  ( L.pure t1'
    <*>. state (readIndirection 1)
    <*>. state (readIndirection 2)
    <*>. state (readIndirection 3)
  ) buf

readT2 :: Buffer ->. (Buffer, T (S n))
readT2 buf = runState
  ( L.pure t2'
    <*>. state (readIndirection 1)
    <*>. state (readIndirection 2)
    <*>. state (readIndirection 3)
    <*>. state (readIndirection 4)
    <*>. state (readIndirection 5)
  ) buf

customWrite :: Heap a => Int -> a ->. Buffer ->. Buffer
customWrite x y z = writeIndirection x z y

($.) :: (a ->. b) ->. a ->. b
f $. a = f a

data R n where
  R1 :: (T n) ->. R n
  R2 :: (T n) ->. !Int -> (T n) ->. R n

insert :: Int -> Set ->. Set
insert x (Set tree) = case ins tree of
  R1 a -> Set a
  R2 a b c -> Set (t1 (R.allocate a) b (R.allocate c))
  where
  ins :: forall n t. T n ->. R n
  ins LF = R2 LF x LF
  ins (BR n) = i n
    where
    i :: forall m. N m ->. R (S m)
    i (T1 a b c) = case compare x b of
      LT -> run2 (R.deallocateExplicit a) c
        where
        run2 :: (Deallocation,T m) ->. Ref (T m) ->. R (S m)
        run2 (dealloc,a') c' = run (ins a') (patchIndirection dealloc c')
        run :: R m ->. Ref (T m) ->. R (S m)
        run (R1 k) c' = R1 (t1 (R.allocate k) b c')
        run (R2 p q r) c' = R1 (t2 (R.allocate p) q (R.allocate r) b c')
      GT -> run2 (R.deallocateExplicit c) a
        where
        run2 :: (Deallocation,T m) ->. Ref (T m) ->. R (S m)
        run2 (dealloc,c') a' = run (ins c') (patchIndirection dealloc a')
        run :: R m ->. Ref (T m) ->. R (S m)
        run (R1 k) a' = R1 (t1 a' b (R.allocate k))
        run (R2 p q r) a' = R1 (t2 a' b (R.allocate p) q (R.allocate r))
      EQ -> run a c
        where
        run :: Ref (T m) ->. Ref (T m) ->. R (S m)
        run a' c' = R1 (t1 a' b c')
    i (T2 a b c d e) = case compare x b of
      LT -> run2 (R.deallocateExplicit a) c e
        where
        run2 :: (Deallocation,T m) ->. Ref (T m) ->. Ref (T m) ->. R (S m)
        run2 (dealloc,a') c' e' = run (ins a') (patchIndirection dealloc c') e'
        run :: R m ->. Ref (T m) ->. Ref (T m) ->. R (S m)
        run (R1 k) c' e' = R1 (t2 (R.allocate k) b c' d e')
        run (R2 p q r) c' e' = R2 (t1 (R.allocate p) q (R.allocate r)) b (t1 c' d e')
      EQ -> run a c e
        where
        run :: Ref (T m) ->. Ref (T m) ->. Ref (T m) ->. R (S m)
        run a' c' e' = R1 (t2 a' b c' d e')
      GT -> case compare x d of
        LT -> run2 (R.deallocateExplicit c) a e
          where
          run2 :: (Deallocation,T m) ->. Ref (T m) ->. Ref (T m) ->. R (S m)
          run2 (dealloc,c') a' e' = run (ins c') (patchIndirection dealloc a') e'
          run :: R m ->. Ref (T m) ->. Ref (T m) ->. R (S m)
          run (R1 k) a' e' = R1 (t2 a' b (R.allocate k) d e')
          run (R2 p q r) a' e' = R2 (t1 a' b (R.allocate p)) q (t1 (R.allocate r) d e')
        EQ -> run a c e
          where
          run :: Ref (T m) ->. Ref (T m) ->. Ref (T m) ->. R (S m)
          run a' c' e' = R1 (t2 a' b c' x e')
        GT -> run2 (R.deallocateExplicit e) a c
          where
          run2 :: (Deallocation,T m) ->. Ref (T m) ->. Ref (T m) ->. R (S m)
          run2 (dealloc,e') a' c' = run (ins e') (patchIndirection dealloc a') c'
          run :: R m ->. Ref (T m) ->. Ref (T m) ->. R (S m)
          run (R1 k) a' c' = R1 (t2 a' b c' d (R.allocate k))
          run (R2 p q r) a' c' = R2 (t1 a' b c') d (t1 (R.allocate p) q (R.allocate r))

-- insert :: forall a. Ord a => a -> Set a ->. Set a
-- insert x (Set tree) = ins tree Set (\a b c -> Set (t1 a b c))
--   where
--     ins :: forall n t. T n a ->. Keep t n a -> Push t n a -> t
--     ins LF = \keep push -> push (R.allocate LF) x (R.allocate LF)
-- 
--     ins (BR n) = i n
--       where
--         i :: forall p m. (S p ~ m) => N p a ->. Keep t m a -> Push t m a -> t
--         i (T2 a b c d e) keep push = select2 x b d xltb xeqb xbtw xeqd xgtd
--           where
--             xltb = ins (R.deallocate a) (\k -> keep (R.allocate (t2 k b c d e))) (\p q r -> push (R.allocate (t1 p q r)) b (R.allocate (t1 c d e)))
--             xbtw = ins (R.deallocate c) (\k -> keep (R.allocate (t2 a b k d e))) (\p q r -> push (R.allocate (t1 a b p)) q (R.allocate (t1 r d e)))
--             xgtd = ins (R.deallocate e) (\k -> keep (R.allocate (t2 a b c d k))) (\p q r -> push (R.allocate (t1 a b c)) d (R.allocate (t1 p q r)))
--             xeqb = keep (R.allocate (t2 a x c d e))
--             xeqd = keep (R.allocate (t2 a b c x e))
-- 
--         i (T1 a b c) keep push = case compare x b of
--           LT -> ins (R.deallocate a) (\k -> keep (R.allocate (t1 k b c))) (\p q r -> keep (R.allocate (t2 p q r b c)))
--           GT -> ins (R.deallocate c) (\k -> keep (R.allocate (t1 a b k))) (\p q r -> keep (R.allocate (t2 a b p q r)))
--           EQ -> keep (R.allocate (t1 a x c))
--           -- select1 x b xltb xeqb xgtb
--           -- where
--           --   xltb = ins (R.deallocate a) (\k -> keep (R.allocate (t1 k b c))) (\p q r -> keep (R.allocate (t2 p q r b c)))
--           --   xgtb = ins (R.deallocate c) (\k -> keep (R.allocate (t1 a b k))) (\p q r -> keep (R.allocate (t2 a b p q r)))
--           --   xeqb = keep (R.allocate (t1 a x c))


t1 :: Ref (T n) ->. Int -> Ref (T n) ->. T (S n)
t1 a b c = BR (T1 a b c)

t2 :: Ref (T n) ->. Int -> Ref (T n) ->. Int -> Ref (T n) ->. T (S n)
t2 a b c d e = BR (T2 a b c d e)

t1' :: Ref (T n) ->. Int ->. Ref (T n) ->. T (S n)
t1' = unsafeCoerce t1

t2' :: Ref (T n) ->. Int ->. Ref (T n) ->. Int ->. Ref (T n) ->. T (S n)
t2' = unsafeCoerce t2


data E a = E0 a | E1 a | E2 a

select1 :: Ord a => a -> a -> b -> b -> b -> b
select1 x y lt eq gt
  = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }

select2 :: Ord a => a -> a -> a -> b -> b -> b -> b -> b -> b
select2 x y z xlty xeqy xbtw xeqz xgtz
  = select1 x y xlty xeqy (select1 x z xbtw xeqz xgtz)

