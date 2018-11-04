{-# language LinearTypes #-}
{-# language MagicHash #-}

module Linear.Class
  ( Functor(..)
  , Applicative(..)
  , Semiapplicative(..)
  , Monad(..)
  , Semigroup(..)
  , Cosemigroup(..)
  , Comonoid(..)
  , Bifunctor(..)
  , (<*>.)
  , (<>.)
  , (>>=.)
  , uncurry
  , flip
  ) where

import Prelude ()
import GHC.Int (Int8(I8#),Int64(I64#))
import GHC.Word (Word8(W8#))

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

instance Bifunctor (,) where
  first f (a,c) = (f a, c)
  {-# INLINE first #-}
  second f (a,b) = (a, f b)
  {-# INLINE second #-}

instance Cosemigroup Int8 where
  coappend (I8# x) = (I8# x, I8# x)

instance Comonoid Int8 where
  coempty (I8# _) = ()

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

