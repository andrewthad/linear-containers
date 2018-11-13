{-# language GADTSyntax #-}
{-# language LinearTypes #-}

module Linear.Reader
  ( Reader(..)
  , reader
  , runReader
  , bindReader
  ) where

import qualified Linear.Class as L

newtype Reader r a where
  Reader :: (r ->. a) ->. Reader r a

runReader :: Reader r a ->. r ->. a
runReader (Reader f) s = f s

reader :: (r ->. a) ->. Reader r a
reader = Reader

instance L.Functor (Reader r) where
  map f (Reader g) = Reader ( \s0 -> f (g s0))

instance L.Cosemigroup r => L.Semiapplicative (Reader r) where
  apply (Reader f) (Reader g) = Reader $. \s0 -> L.uncurry
    (\s1 s2 -> f s1 (g s2))
    (L.coappend s0)

instance L.Comonoid r => L.Applicative (Reader r) where
  pure a = Reader (\s -> ununit (L.coempty s) a)

instance L.Comonoid r => L.Monad (Reader r) where
  bind = bindReader

-- | This is provided because the @Monad@ instance for @Reader@
--   requires a stronger constraint than what is actually needed
--   to provide a bind function.
bindReader :: L.Cosemigroup r => Reader r a ->. (a ->. Reader r b) ->. Reader r b
bindReader (Reader f) g = Reader $. \s0 -> L.uncurry
  (\s1 s2 -> runReader (g (f s1)) s2
  ) (L.coappend s0)

ununit :: () ->. a ->. a
ununit () a = a

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a

