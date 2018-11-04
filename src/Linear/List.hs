{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}

module Linear.List
  ( List(..)
  , mapReference
  , map
  , map'
  ) where

import Prelude hiding (map)

import Linear.Class ((>>=.),(<*>.),(<>.))
import Linear.Types (Object,Reference,Mode,PrimObject(..),Mode(..),Token)
import Data.Primitive (Addr)
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import GHC.Exts (Int(..))
import GHC.Int (Int64(..))
import Linear.State (state,runState)
import qualified Linear.Class as C
import qualified Linear.Types as L
import qualified Linear.Reference as R
import qualified Data.Primitive as PM

-- | A linked list that must be consumed linearly.
data List :: (Mode -> Type) -> Mode -> Type where
  Cons :: {-# UNPACK #-} !(Reference (List f) m) ->. !(f m) ->. List f m
  Nil :: List f m

instance Object f => Object (List f) where
  size _ = (2 * wordSize) + L.size (Proxy :: Proxy f)
  -- Fix this Int64 madness
  poke !addr Nil !t0 = L.poke addr (PrimObject (0 :: Int64)) t0
  poke !addr (Cons ref a) !t0 = 
       L.poke (PM.plusAddr addr (wordSize + wordSize)) a
    $. L.poke (PM.plusAddr addr wordSize) ref
    $. L.poke addr (PrimObject (1 :: Int64)) t0
  -- TODO: Using Int64 here is a hack. We should be using machine
  -- integers instead, but GHC will not cooperate.
  peek !addr = runState $. state (L.peek addr) >>=. \(PrimObject (I64# x)) -> case x of
    0# -> C.pure Nil
    _ -> C.pure Cons
      <*>. state (L.peek (PM.plusAddr addr wordSize))
      <*>. state (L.peek (PM.plusAddr addr (wordSize + wordSize)))
  forget Nil !t = t
  forget (Cons ref a) !t = R.ignore ref <>. L.forget a t

-- | Map over a list, updating the elements in-place.
map :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List f 'Dynamic
  ->. List f 'Dynamic
map g Nil = Nil
map g (Cons ref x) = Cons 
  ( R.statically_
    ref
    (\xs t -> mapHelp2 g xs t)
  ) x

-- | Map over a reference to a list, updating the elements in-place.
map' :: forall f. Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  Reference (List f) 'Static
  ->. Token
  ->. Token
map' g ref t0 = 
  C.uncurry (\t xs -> mapHelp2 g xs (t0 <>. t)) (R.read (R.modify ref (mapHelp1 g)))

mapHelp2 :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List f 'Static
  ->. Token
  ->. Token
mapHelp2 g Nil !t = t
mapHelp2 g (Cons xs a) !t = map' g xs (L.forget a t)

mapHelp1 :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List f 'Dynamic
  ->. List f 'Dynamic
mapHelp1 g Nil = Nil
mapHelp1 g (Cons xs a) = Cons xs (g a)

-- I need to hook this up to a rewrite rule for map. Its just
-- a more efficient version of map specialized to a reference
-- element type. It is able to be more efficient because we
-- know not to overwrite the cons cells in the map, since the
-- references stay the same.
mapReference :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List (Reference f) 'Dynamic
  ->. List (Reference f) 'Dynamic
mapReference g Nil = Nil
mapReference g (Cons xsRefD xRef) = Cons
  (R.statically_ xsRefD (\xs t0 -> mapReferenceGo g xs t0))
  (R.modify xRef g)

mapReferenceGo :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List (Reference f) 'Static
  ->. Token
  ->. Token
mapReferenceGo g Nil !t0 = t0
mapReferenceGo g (Cons xsRef xRef) !t0 = 
  C.uncurry (\t1 xsNext -> mapReferenceGo g xsNext (t0 <>. t1 <>. R.dynamically_ xRef g)) (R.read xsRef)

wordSize :: Int
wordSize = PM.sizeOf (undefined :: Addr)

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a
 
