{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Linear.Set
  ( Set
  ) where

import Data.Linear.Types (Ref,Indirection(..))
import Data.Primitive (Addr)
import qualified Data.Linear.Ref as R
import qualified Data.Primitive as PM

data Set a where
  Set :: T n a -> Set a

data Nat = Z | S Nat

data N n a where
  T1 :: Ref (T n a) ->. a -> Ref (T n a) ->. N n a
  T2 :: Ref (T n a) ->. a -> Ref (T n a) ->. a -> Ref (T n a) ->. N n a

data T n a where
  BR :: N n a ->. T (S n) a
  LF :: T Z a

instance (a ~ Int) => Indirection (T n a) where
  sizeIndirection _ = (PM.sizeOf (undefined :: Addr) * 6)
  writeIndirection off buf LF =
    writeIndirection 0 buf (0 :: Int)

type Keep t n a = Ref (T n a) ->. t
type Push t n a = Ref (T n a) ->. a -> Ref (T n a) ->. t

data R n a where
  R1 :: (T n a) ->. R n a
  R2 :: (T n a) ->. a -> (T n a) ->. R n a

insert :: forall a. Ord a => a -> Set a ->. Set a
insert x (Set tree) = case ins tree of
  R1 a -> Set a
  R2 a b c -> Set (t1 (R.allocate a) b (R.allocate c))
  where
  ins :: forall n t. T n a ->. R n a
  ins LF = R2 LF x LF
  ins (BR n) = i n
    where
    i :: forall m. N m a ->. R (S m) a
    i (T1 a b c) = case compare x b of
      LT -> run (ins (R.deallocate a)) c
        where
        run :: R m a ->. Ref (T m a) ->. R (S m) a
        run (R1 k) c' = R1 (t1 (R.allocate k) b c')
        run (R2 p q r) c' = R1 (t2 (R.allocate p) q (R.allocate r) b c')
      GT -> run (ins (R.deallocate c)) a
        where
        run :: R m a ->. Ref (T m a) ->. R (S m) a
        run (R1 k) a' = R1 (t1 a' b (R.allocate k))
        run (R2 p q r) a' = R1 (t2 a' b (R.allocate p) q (R.allocate r))
      EQ -> run a c
        where
        run :: Ref (T m a) ->. Ref (T m a) ->. R (S m) a
        run a' c' = R1 (t1 a' b c')
    i (T2 a b c d e) = case compare x b of
      LT -> run (ins (R.deallocate a)) c e
        where
        run :: R m a ->. Ref (T m a) ->. Ref (T m a) ->. R (S m) a
        run (R1 k) c' e' = R1 (t2 (R.allocate k) b c' d e')
        run (R2 p q r) c' e' = R2 (t1 (R.allocate p) q (R.allocate r)) b (t1 c' d e')
      EQ -> run a c e
        where
        run :: Ref (T m a) ->. Ref (T m a) ->. Ref (T m a) ->. R (S m) a
        run a' c' e' = R1 (t2 a' b c' d e')
      GT -> case compare x d of
        LT -> run (ins (R.deallocate c)) a e
          where
          run :: R m a ->. Ref (T m a) ->. Ref (T m a) ->. R (S m) a
          run (R1 k) a' e' = R1 (t2 a' b (R.allocate k) d e')
          run (R2 p q r) a' e' = R2 (t1 a' b (R.allocate p)) q (t1 (R.allocate r) d e')
        EQ -> run a c e
          where
          run :: Ref (T m a) ->. Ref (T m a) ->. Ref (T m a) ->. R (S m) a
          run a' c' e' = R1 (t2 a' b c' x e')
        GT -> run (ins (R.deallocate e)) a c
          where
          run :: R m a ->. Ref (T m a) ->. Ref (T m a) ->. R (S m) a
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


t1 :: Ref (T n a) ->. a -> Ref (T n a) ->. T (S n) a
t1 a b c = BR (T1 a b c)

t2 :: Ref (T n a) ->. a -> Ref (T n a) ->. a -> Ref (T n a) ->. T (S n) a
t2 a b c d e = BR (T2 a b c d e)

data E a = E0 a | E1 a | E2 a

select1 :: Ord a => a -> a -> b -> b -> b -> b
select1 x y lt eq gt
  = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }

select2 :: Ord a => a -> a -> a -> b -> b -> b -> b -> b -> b
select2 x y z xlty xeqy xbtw xeqz xgtz
  = select1 x y xlty xeqy (select1 x z xbtw xeqz xgtz)

