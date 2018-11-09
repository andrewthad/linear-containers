{-# language DeriveFunctor #-}
{-# language GADTSyntax #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}

module Linear.Class
  ( Functor(..)
  , Applicative(..)
  , Semiapplicative(..)
  , Monad(..)
  , Semigroup(..)
  , Cosemigroup(..)
  , Comonoid(..)
  , Bifunctor(..)
  , Movable(..)
  , Unrestricted(..)
  , (<*>.)
  , (<>.)
  , (>>=.)
  , getUnrestricted
  , uncurry
  , flip
  , coappend3
  ) where

import Prelude (Show)
import GHC.Int (Int8(I8#),Int32(I32#),Int64(I64#))
import GHC.Word (Word8(W8#))

import qualified Prelude as P

class Functor f where
  map :: (a ->. b) ->. f a ->. f b

class Functor f => Semiapplicative f where
  apply :: f (a ->. b) ->. f a ->. f b

class Semiapplicative f => Applicative f where
  pure :: a ->. f a

class Applicative f => Monad f where
  bind :: f a ->. (a ->. f b) ->. f b

class Semigroup a where
  append :: a ->. a ->. a

class Cosemigroup a where
  coappend :: a ->. (a,a)

class Cosemigroup a => Comonoid a where
  coempty :: a ->. ()

class Bifunctor f where
  first :: (a ->. b) ->. f a c ->. f b c
  second :: (b ->. c) ->. f a b ->. f a c
  bimap :: (a ->. b) ->. (c ->. d) ->. f a c ->. f b d
  bimap f g x = first f (second g x)

-- The reader instance causes a parser error. Need to wait for linear haskell
-- to have this fixed.
--
-- instance Cosemigroup a => Semiapplicative ((->.) a) where
--   apply f g a = uncurry
--     (\a1 a2 -> f a1 (g a2)
--     ) (coappend a)

instance Bifunctor (,) where
  first f (a,c) = (f a, c)
  {-# INLINE first #-}
  second f (a,b) = (a, f b)
  {-# INLINE second #-}

instance Cosemigroup a => Cosemigroup [a] where
  coappend [] = ([],[])
  coappend (x : xs) = bimap (uncurry (:)) (uncurry (:)) (coappend (x,xs))

instance (Cosemigroup a, Cosemigroup b) => Cosemigroup (a,b) where
  coappend (a,b) = swapTuples (coappend a) (coappend b)

instance (Cosemigroup a, Cosemigroup b, Cosemigroup c) => Cosemigroup (a,b,c) where
  coappend (a,b,c) = swapTriples (coappend a) (coappend b) (coappend c)

instance Cosemigroup Int8 where
  coappend (I8# x) = (I8# x, I8# x)

instance Comonoid Int8 where
  coempty (I8# _) = ()

instance Cosemigroup Int32 where
  coappend (I32# x) = (I32# x, I32# x)

instance Comonoid Int32 where
  coempty (I32# _) = ()

instance Cosemigroup Int64 where
  coappend (I64# x) = (I64# x, I64# x)

instance Comonoid Int64 where
  coempty (I64# _) = ()

infixl 4 <*>.
(<*>.) :: Semiapplicative f => f (a ->. b) ->. f a ->. f b
(<*>.) = apply

infixr 6 <>.
(<>.) :: Semigroup a => a ->. a ->. a
(<>.) = append

infixl 1 >>=.
(>>=.) :: Monad f => f a ->. (a ->. f b) ->. f b
(>>=.) = bind

uncurry :: (a ->. b ->. c) ->. (a,b) ->. c
uncurry f (a,b) = f a b

flip :: (a ->. b ->. c) ->. b ->. a ->. c
flip f b a = f a b

-- | An unrestricted value.
data Unrestricted a where
  Unrestricted :: a -> Unrestricted a
  deriving (Show,P.Functor)

getUnrestricted :: Unrestricted a -> a
getUnrestricted (Unrestricted a) = a

class Movable a where
  move :: a ->. Unrestricted a

instance (Movable a, Movable b) => Movable (a,b) where
  move (a,b) = go (move a) (move b)
    where go :: Unrestricted a ->. Unrestricted b ->. Unrestricted (a,b)
          go (Unrestricted a) (Unrestricted b) = Unrestricted (a,b)

instance Movable a => Movable [a] where
  move [] = Unrestricted []
  move (x : xs) = go (move x) (move xs)
    where go :: Unrestricted a ->. Unrestricted [a] ->. Unrestricted [a]
          go (Unrestricted a) (Unrestricted as) = Unrestricted (a : as)

instance Movable Int32 where
  move (I32# x) = Unrestricted (I32# x)

coappend3 :: Cosemigroup a => a ->. (a,a,a)
coappend3 a = uncurry (\b c -> uncurry (\d e -> (c,d,e)) (coappend b)) (coappend a)

swapTuples :: (a,a) ->. (b,b) ->. ((a,b),(a,b))
swapTuples (a1,a2) (b1,b2) = ((a1,b1),(a2,b2))

swapTriples :: (a,a) ->. (b,b) ->. (c,c) ->. ((a,b,c),(a,b,c))
swapTriples (a1,a2) (b1,b2) (c1,c2) = ((a1,b1,c1),(a2,b2,c2))

liftA2 :: Semiapplicative f => (a ->. b ->. c) ->. f a ->. f b ->. f c
liftA2 f x = (<*>.) (map f x)

