{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}

module Linear.List
  ( List(..)
  , ListI(..)
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
  , toPrim
  , fromPrim
  ) where

import Prelude hiding (map,foldl,replicate)

import Linear.Class ((>>=.),(<*>.),(<>.))
import Linear.Types (Object,Reference,Mode,PrimObject(..),Mode(..),Token)
import Data.Primitive (Addr,Prim)
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import GHC.Exts (Int(..))
import GHC.Int (Int64(..))
import Linear.Reader (reader,runReader,bindReader)
import qualified Linear.Class as C
import qualified Linear.Types as L
import qualified Linear.Reference as R
import qualified Data.Primitive as PM
import qualified Data.List as List

-- | A singly-linked list that must be consumed linearly.
data List :: (Mode -> Type) -> Mode -> Type where
  List :: ListI f m ->. Token ->. List f m

data ListI :: (Mode -> Type) -> Mode -> Type where
  Cons :: {-# UNPACK #-} !(Reference (List f) m) ->. !(f m) ->. ListI f m
  Nil :: ListI f m

data Cell :: (Mode -> Type) -> Mode -> Type where
  Cell :: {-# UNPACK #-} !(Reference (List f) m) ->. !(f m) ->. Cell f m

instance Object f => Object (List f) where
  size _ = (2 * wordSize) + L.size (Proxy :: Proxy f)
  -- Fix this Int64 madness
  poke !addr (List Nil t0) = L.poke addr (PrimObject (0 :: Int64) t0)
  poke !addr (List (Cons ref a) t0) = 
    L.poke (PM.plusAddr addr (wordSize + wordSize)) (L.inhume (L.poke (PM.plusAddr addr wordSize) (L.inhume (L.poke addr (PrimObject (1 :: Int64) t0)) ref)) a)
  -- TODO: Using Int64 here is a hack. We should be using machine
  -- integers instead, but GHC will not cooperate.
  peek :: forall (m :: Mode). Addr -> Token ->. List f m
  peek !addr !t0 = go (L.peek addr t0)
    where
    go :: PrimObject Int64 m ->. List f m
    go (PrimObject (I64# x) t1) = runReader
      ( case x of
          0# -> reader (List Nil)
          _ -> C.liftA2 (\x y -> C.uncurry (\ty y' -> C.uncurry (\tx x' -> (List (Cons x' y') (tx <>. ty))) (L.exhume x)) (L.exhume y))
            (reader (L.peek (PM.plusAddr addr wordSize)))
            (reader (L.peek (PM.plusAddr addr (wordSize + wordSize))))
      ) t1
  forget (List Nil t0) = t0
  forget (List (Cons ref a) t0) = R.ignore ref <>. L.forget a <>. t0
  inhume t0 (List x t1) = List x (t0 <>. t1)
  exhume (List x t0) = C.second (List x) (C.coappend t0)
  exhumeAll (List Nil t0) = C.second (List Nil) (C.coappend t0)
  exhumeAll (List (Cons r v) t0) = C.liftA3 (\x y z -> List (Cons x y) z)
    (L.exhumeAll r)
    (L.exhumeAll v)
    (C.coappend t0)

-- | The empty list
nil :: Token ->. List f 'Dynamic
nil = List Nil

-- | /O(1)/ Push a new element onto the head of the list.
cons :: Object f
  =>  f 'Dynamic
  ->. List f 'Dynamic
  ->. List f 'Dynamic
cons x xs = C.uncurry (\t ref -> List (Cons ref x) t) (L.exhume (R.allocate xs))

uncons :: Object f
  =>  List f 'Dynamic
  ->. Either Token (f 'Dynamic, List f 'Dynamic)
uncons (List Nil t0) = Left t0
uncons (List (Cons ref x) t0) = Right (x,L.inhume t0 (R.deallocate ref))

append :: Object f
  =>  List f 'Dynamic
  ->. List f 'Dynamic
  ->. List f 'Dynamic
append (List Nil t0) ys = L.inhume t0 ys
append (List (Cons r x) t0) ys = List
  ( Cons
    ( R.statically_ r
      (\s -> R.descend s sumOfProducts
        (\(Cell ref obj) -> L.inhume (L.forget obj) ref)
        (\(Token1 t) -> L.inhume t ys)
      )
    ) x
  ) t0

newtype Token1 :: Mode -> Type where
  Token1 :: Token ->. Token1 m

sumOfProducts :: List f m ->. Either (Cell f m) (Token1 m)
sumOfProducts (List Nil t0) = Right (Token1 t0)
sumOfProducts (List (Cons r obj) t0) = Left (Cell (L.inhume t0 r) obj)

-- | Map over a list, updating the elements in-place.
map :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List f 'Dynamic
  ->. List f 'Dynamic
map g (List Nil t0) = List Nil t0
map g (List (Cons ref x) t0) = List
  ( Cons ( R.statically_ ref (\refS -> map' g refS)) (g x)
  ) t0

-- | Map over a reference to a list, updating the elements in-place.
map' :: forall f. Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  Reference (List f) 'Static
  ->. Token
map' g ref = R.burrow ref sumOfProducts
  (cellNext g)
  (\(Token1 t) -> List Nil t)

-- This lens is used by map.
cellNext :: (f 'Dynamic ->. f 'Dynamic) ->. C.Lens (Cell f 'Dynamic) (List f 'Dynamic) (Reference (List f) 'Dynamic) (Reference (List f) 'Dynamic)
cellNext g k (Cell ref x) = C.map (\ref' -> C.uncurry (\t ref'' -> List (Cons ref'' (g x)) t) (L.exhume ref')) (k ref)

-- 
--   -- C.uncurry (\t xs -> mapHelp2 g xs (t0 <>. t)) (R.read (R.modify ref (mapHelp1 g)))
-- 
-- mapHelp2 :: Object f
--   =>  (f 'Dynamic ->. f 'Dynamic)
--   ->  List f 'Static
--   ->. Token
--   ->. Token
-- mapHelp2 g (Nil t0) !t1 = t0 <>. t1
-- mapHelp2 g (Cons xs a) !t = map' g xs (L.forget a t)
-- 
-- mapHelp1 :: Object f
--   =>  (f 'Dynamic ->. f 'Dynamic)
--   ->  List f 'Dynamic
--   ->. List f 'Dynamic
-- mapHelp1 g (Nil t0) = Nil t0
-- mapHelp1 g (Cons xs a) = Cons xs (g a)

-- I need to hook this up to a rewrite rule for map. Its just
-- a more efficient version of map specialized to a reference
-- element type. It is able to be more efficient because we
-- know not to overwrite the cons cells in the map, since the
-- references stay the same.
mapReference :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List (Reference f) 'Dynamic
  ->. List (Reference f) 'Dynamic
mapReference g (List Nil t0) = List Nil t0
mapReference g (List (Cons xsRefD xRef) t0) = List
  ( Cons
    (R.statically_ xsRefD (\xsRef -> mapReferenceGo g (R.read xsRef)))
    (R.modify xRef g)
  ) t0

mapReferenceGo :: Object f
  =>  (f 'Dynamic ->. f 'Dynamic)
  ->  List (Reference f) 'Static
  ->. Token
mapReferenceGo g (List Nil t0) = t0
mapReferenceGo g (List (Cons xsRef xRef) t0) = 
  mapReferenceGo g (R.read (L.inhume (t0 <>. R.dynamically_ xRef g) xsRef))

-- | Left fold over the list, strict in the accumulator. This deallocates
--   the list as it walks over it.
foldl :: Object f
  =>  (b ->. f 'Dynamic ->. b)
  ->  b
  ->. List f 'Dynamic
  ->. (Token,b)
foldl g !acc (List Nil t) = (t,acc)
foldl g !acc (List (Cons xsRef x) t) = foldl g (g acc x) (R.deallocate (L.inhume t xsRef))

-- | Left fold over the list, strict in the accumulator. This preserves
--   the list.
sfoldl :: Object f
  =>  (b ->. f 'Static ->. b)
  ->  b
  ->. List f 'Static
  ->. (Token,b)
sfoldl g !acc (List Nil t) = (t,acc)
sfoldl g !acc (List (Cons xsRef x) t) = sfoldl g (g acc x) (R.read (L.inhume t xsRef))

replicate :: (Prim a)
  =>  Int
  ->  a
  ->  Token
  ->. List (PrimObject a) 'Dynamic
replicate n a t = C.uncurry
  (\t' t'' -> C.uncurry L.inhume (replicateGo n a (nil t') t''))
  (C.coappend t)

replicateGo :: (Prim a)
  =>  Int
  ->  a
  ->  List (PrimObject a) 'Dynamic
  ->. Token
  ->. (Token, List (PrimObject a) 'Dynamic)
replicateGo n a xs t = if n > 0
  then C.uncurry (\t' t'' -> replicateGo (n - 1) a (cons (PrimObject a t') xs) t'') (C.coappend t)
  else (t,xs)

-- | Convert a list on the managed, nonlinear heap to a list on the
--   linear heap. This new list does not contribute to garbage collector
--   pauses.
fromNonlinear :: Object f => [f 'Dynamic] ->. Token ->. List f 'Dynamic
fromNonlinear [] t = List Nil t
fromNonlinear (x : xs) t = cons x (fromNonlinear xs t)

fromPrim :: Prim a => [a] -> Token ->. List (PrimObject a) 'Dynamic
fromPrim [] t = List Nil t
fromPrim (x : xs) t = C.uncurry (\t' t'' -> cons (PrimObject x t') (fromPrim xs t'')) (C.coappend t)

toNonlinear :: Object f => List f 'Dynamic ->. (Token, [f 'Dynamic])
toNonlinear xs = toNonlinearStep (uncons xs) 

toNonlinearStep :: Object f => Either Token (f 'Dynamic, List f 'Dynamic) ->. (Token, [f 'Dynamic])
toNonlinearStep (Left t0) = (t0,[])
toNonlinearStep (Right (x,xs)) = C.uncurry (\t0 ys -> (t0,x : ys)) (toNonlinear xs)

toPrim :: Prim a => List (PrimObject a) 'Dynamic ->. (Token, [a])
toPrim xs = toPrimStep (uncons xs)

toPrimStep :: Prim a => Either Token (PrimObject a 'Dynamic, List (PrimObject a) 'Dynamic) ->. (Token, [a])
toPrimStep (Left t0) = (t0,[])
toPrimStep (Right (PrimObject x t0,xs)) = C.uncurry (\t1 ys -> (t0 <>. t1,x : ys)) (toPrim xs)

wordSize :: Int
wordSize = PM.sizeOf (undefined :: Addr)

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a





