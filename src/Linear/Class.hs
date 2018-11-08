{-# language GADTSyntax #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}

module Linear.Class
  ( Functor(..)
  , Applicative(..)
  , Semiapplicative(..)
  , Unzip(..)
  , Semidecisive(..)
  , Monad(..)
  , Semigroup(..)
  , Monoid(..)
  , Cosemigroup(..)
  , Comonoid(..)
  , Movable(..)
  , Bifunctor(..)
  , Unrestricted(..)
  , (<*>.)
  , (<>.)
  , (>>=.)
  , uncurry
  , flip
  , getUnrestricted
  ) where

import Prelude (Either,Show)
import GHC.Int (Int8(I8#),Int32(I32#),Int64(I64#))
import GHC.Word (Word8(W8#))

class Functor f where
  map :: (a ->. b) ->. f a ->. f b

class Functor f => Semiapplicative f where
  apply :: f (a ->. b) ->. f a ->. f b

-- | In a nonlinear setting, unzip is trivially definable for all
--   functors. However, in a linear setting, functor is not strong
--   enough to capture this.
class Functor f => Unzip f where
  unzip :: f (a,b) ->. (f a, f b)

-- | @Semidecisive@ is dual to @Semiapplicative@.
class Functor f => Semidecisive f where
  decide :: f (Either a b) ->. Either (f a) (f b)

class Semiapplicative f => Applicative f where
  pure :: a ->. f a

class Applicative f => Monad f where
  bind :: f a ->. (a ->. f b) ->. f b

class Semigroup a where
  append :: a ->. a ->. a

class Semigroup a => Monoid a where
  empty :: a

class Cosemigroup a where
  coappend :: a ->. (a,a)

class Cosemigroup a => Comonoid a where
  coempty :: a ->. ()

class Movable a where
  move :: a ->. Unrestricted a

instance Movable Int32 where
  move (I32# x) = Unrestricted (I32# x)

class Bifunctor f where
  first :: (a ->. b) ->. f a c ->. f b c
  second :: (b ->. c) ->. f a b ->. f a c

instance Bifunctor (,) where
  first f (a,c) = (f a, c)
  {-# INLINE first #-}
  second f (a,b) = (a, f b)
  {-# INLINE second #-}

instance Cosemigroup Int8 where
  coappend (I8# x) = (I8# x, I8# x)

instance Cosemigroup Int32 where
  coappend (I32# x) = (I32# x, I32# x)

instance Comonoid Int8 where
  coempty (I8# _) = ()

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
  deriving (Show)

getUnrestricted :: Unrestricted a -> a
getUnrestricted (Unrestricted a) = a

