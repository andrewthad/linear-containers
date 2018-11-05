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
import Linear.Types (Object,Referential,Reference,Mode,PrimObject(..),Mode(..),Token)
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
  Nil :: {-# UNPACK #-} !Token ->. List f m

instance Object f => Object (List f) where
  size _ = (2 * wordSize) + L.size (Proxy :: Proxy f)
  -- Fix this Int64 madness
  poke !addr (Nil t0) !t1 = L.poke addr (PrimObject (0 :: Int64)) (t0 <>. t1)
  poke !addr (Cons ref a) !t0 = 
       L.poke (PM.plusAddr addr (wordSize + wordSize)) a
    $. L.poke (PM.plusAddr addr wordSize) ref
    $. L.poke addr (PrimObject (1 :: Int64)) t0
  -- TODO: Using Int64 here is a hack. We should be using machine
  -- integers instead, but GHC will not cooperate.
  peek !addr = runState $. state (L.peek addr) >>=. \(PrimObject (I64# x)) -> case x of
    0# -> state (\t -> (C.uncurry (\t0 t1 -> (t0, Nil t1)) (C.coappend t)))
    _ -> C.pure Cons
      <*>. state (L.peek (PM.plusAddr addr wordSize))
      <*>. state (L.peek (PM.plusAddr addr (wordSize + wordSize)))
  forget (Nil t0) !t1 = t0 <>. t1
  forget (Cons ref a) !t = R.ignore ref <>. L.forget a t

instance Object f => Referential (List f) where
  inhume t0 (Nil t1) = Nil (t0 <>. t1)
  inhume t0 (Cons ref x) = Cons (L.inhume t0 ref) x
  exhume (Nil t0) = C.second (\t1 -> Nil t1) (C.coappend t0)
  exhume (Cons ref x) = C.second (\r -> Cons r x) (L.exhume ref)

-- | /O(1)/ Push a new element onto the head of the list.
cons :: Object f
  => f 'Dynamic
  -> List f 'Dynamic
  -> List f 'Dynamic
cons x xs = Cons (R.allocate' xs) x

uncons :: Object f
  =>  List f 'Dynamic
  ->. Either Token (f 'Dynamic, List f 'Dynamic)
uncons (Nil t0) = Left t0
uncons (Cons ref x) = C.uncurry (\t0 xs -> Right (x,L.inhume t0 xs)) (R.deallocate ref)

-- | Map over a list, updating the elements in-place.
map :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List f 'Dynamic
  ->. List f 'Dynamic
map g (Nil t0) = (Nil t0)
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
mapHelp2 g (Nil t0) !t1 = t0 <>. t1
mapHelp2 g (Cons xs a) !t = map' g xs (L.forget a t)

mapHelp1 :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List f 'Dynamic
  ->. List f 'Dynamic
mapHelp1 g (Nil t0) = Nil t0
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
mapReference g (Nil t0) = Nil t0
mapReference g (Cons xsRefD xRef) = Cons
  (R.statically_ xsRefD (\xs t0 -> mapReferenceGo g xs t0))
  (R.modify xRef g)

mapReferenceGo :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List (Reference f) 'Static
  ->. Token
  ->. Token
mapReferenceGo g (Nil t0) !t1 = t0 <>. t1
mapReferenceGo g (Cons xsRef xRef) !t0 = 
  C.uncurry (\t1 xsNext -> mapReferenceGo g xsNext (t0 <>. t1 <>. R.dynamically_ xRef g)) (R.read xsRef)

wordSize :: Int
wordSize = PM.sizeOf (undefined :: Addr)

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a

