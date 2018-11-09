{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}

module Linear.List
  ( List(..)
  , cons
  , uncons
  , mapReference
  , map
  , map'
  , foldl
  , replicate
  , append
  , fromNonlinear
  , toNonlinear
  ) where

import Prelude hiding (map,foldl,replicate)

import Linear.Class ((>>=.),(<*>.),(<>.))
import Linear.Types (Object,Referential,Reference,Mode,PrimObject(..),Mode(..),Token)
import Data.Primitive (Addr)
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import GHC.Exts (Int(..))
import GHC.Int (Int64(..))
import Linear.State (state,runState)
import Data.Functor.Product (Product(Pair))
import qualified Linear.Class as C
import qualified Linear.Types as L
import qualified Linear.Reference as R
import qualified Data.Primitive as PM
import qualified Data.List as List

-- | A singly-linked list that must be consumed linearly.
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
  peeking !addr !t0 f = L.peeking addr t0
    (\t1 (PrimObject (I64# x)) -> case x of
      0# -> C.uncurry (\t2 t3 -> f t2 (Nil t3)) (C.coappend t1)
      _ -> L.peeking (PM.plusAddr addr wordSize) t1
        (\t2 ref -> L.peeking (PM.plusAddr addr (wordSize + wordSize)) t2
          (\t3 payload -> f t3 (Cons ref payload)
          )
        )
    )
  forget (Nil t0) !t1 = t0 <>. t1
  forget (Cons ref a) !t = R.ignore ref <>. L.forget a t

instance Object f => Referential (List f) where
  inhume t0 (Nil t1) = Nil (t0 <>. t1)
  inhume t0 (Cons ref x) = Cons (L.inhume t0 ref) x
  exhume (Nil t0) = C.second (\t1 -> Nil t1) (C.coappend t0)
  exhume (Cons ref x) = C.second (\r -> Cons r x) (L.exhume ref)

-- | The empty list
nil :: Token ->. List f 'Dynamic
nil = Nil

-- | /O(1)/ Push a new element onto the head of the list.
cons :: Object f
  =>  f 'Dynamic
  ->. List f 'Dynamic
  ->. List f 'Dynamic
cons x xs = Cons (R.allocate' xs) x

uncons :: Object f
  =>  List f 'Dynamic
  ->. Either Token (f 'Dynamic, List f 'Dynamic)
uncons (Nil t0) = Left t0
uncons (Cons ref x) = C.uncurry (\t0 xs -> Right (x,L.inhume t0 xs)) (R.deallocate ref)

append :: Object f
  =>  List f 'Dynamic
  ->. List f 'Dynamic
  ->. List f 'Dynamic
append (Nil t0) ys = L.inhume t0 ys
append (Cons r x) ys = Cons
  ( R.statically_ r
    (\s -> R.descend s sumOfProducts
      (\(Pair ref obj) -> C.uncurry L.inhume (C.first (L.forget obj) (L.exhume ref)))
      (\(Token1 t) -> L.inhume t ys)
    )
  ) x

newtype Token1 :: Mode -> Type where
  Token1 :: Token ->. Token1 m

sumOfProducts :: List f m ->. Either (Product (Reference (List f)) f m) (Token1 m)
sumOfProducts (Nil t) = Right (Token1 t)
sumOfProducts (Cons r obj) = Left (Pair r obj)

-- | Map over a list, updating the elements in-place.
map :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List f 'Dynamic
  ->. List f 'Dynamic
map g (Nil t0) = (Nil t0)
map g (Cons ref x) = Cons 
  ( R.statically_
    ref
    (\refS -> C.uncurry (\t xs -> mapHelp2 g xs t) (R.read refS))
  ) (g x)

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
  (R.statically_ xsRefD (\xsRef -> C.uncurry (\t0 xs -> mapReferenceGo g xs t0) (R.read xsRef)))
  (R.modify xRef g)

mapReferenceGo :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List (Reference f) 'Static
  ->. Token
  ->. Token
mapReferenceGo g (Nil t0) !t1 = t0 <>. t1
mapReferenceGo g (Cons xsRef xRef) !t0 = 
  C.uncurry (\t1 xsNext -> mapReferenceGo g xsNext (t0 <>. t1 <>. R.dynamically_ xRef g)) (R.read xsRef)

foldl :: Object f
  =>  (b ->. f 'Dynamic ->. b)
  ->  b
  ->. List f 'Dynamic
  ->. (Token,b)
foldl g !acc (Nil t) = (t,acc)
foldl g !acc (Cons xsRef x) = C.uncurry (\t0 xs -> foldl g (g acc x) (L.inhume t0 xs)) (R.deallocate xsRef)

replicate :: Object f
  =>  Int
  ->  f 'Dynamic
  ->  Token
  ->. List f 'Dynamic
replicate n a t = replicateGo n a (nil t)

replicateGo :: Object f
  =>  Int
  ->  f 'Dynamic
  ->  List f 'Dynamic
  ->. List f 'Dynamic
replicateGo n a xs = if n > 0
  then replicateGo (n - 1) a (cons a xs)
  else xs 

-- | Convert a list on the managed, nonlinear heap to a list on the
--   linear heap. This new list does not contribute to garbage collector
--   pauses.
fromNonlinear :: Object f => [f 'Dynamic] ->. Token ->. List f 'Dynamic
fromNonlinear [] t = Nil t
fromNonlinear (x : xs) t = cons x (fromNonlinear xs t)

toNonlinear :: Object f => List f 'Dynamic ->. (Token, [f 'Dynamic])
toNonlinear xs = toNonlinearStep (uncons xs) 

toNonlinearStep :: Object f => Either Token (f 'Dynamic, List f 'Dynamic) ->. (Token, [f 'Dynamic])
toNonlinearStep (Left t0) = (t0,[])
toNonlinearStep (Right (x,xs)) = C.uncurry (\t0 ys -> (t0,x : ys)) (toNonlinear xs)

wordSize :: Int
wordSize = PM.sizeOf (undefined :: Addr)

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a





