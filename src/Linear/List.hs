{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}

module Linear.List
  ( List(..)
  , nil
  , cons
  , replicate
  , uncons
  , foldl
  , map
  , mapReference
  ) where

import Prelude hiding (map,replicate,foldl)

import Linear.Class (Unrestricted(..),(>>=.),(<*>.),(<>.))
import Linear.Types (Object,Referential,Reference,PrimObject(..),Token,Static)
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

data List a = List {-# UNPACK #-} !Token !(ListI a)

-- | A singly-linked list that must be consumed linearly.
data ListI :: Type -> Type where
  Cons :: {-# UNPACK #-} !(Reference (ListI a)) ->. !a ->. ListI a
  Nil :: ListI a

instance Object a => Object (ListI a) where
  size _ = (2 * wordSize) + L.size (Proxy :: Proxy a)
  -- Fix this Int64 madness
  poke !addr Nil !t0 = L.poke addr (PrimObject (0 :: Int64)) t0
  poke !addr (Cons ref a) !t0 = 
       L.poke (PM.plusAddr addr (wordSize + wordSize)) a
    $. L.poke (PM.plusAddr addr wordSize) ref
    $. L.poke addr (PrimObject (1 :: Int64)) t0
  -- TODO: Using Int64 here is a hack. We should be using machine
  -- integers instead, but GHC will not cooperate.
  peek !addr = runState $. state (L.peek addr) >>=. \(PrimObject (I64# x)) -> case x of
    0# -> state (\t -> (t,Nil))
    _ -> C.pure Cons
      <*>. state (L.peek (PM.plusAddr addr wordSize))
      <*>. state (L.peek (PM.plusAddr addr (wordSize + wordSize)))
  ignore s = ignoreHelp (C.decide (C.map sumOfProducts s))

sumOfProducts :: ListI a ->. Either () (Reference (ListI a), a)
sumOfProducts Nil = Left ()
sumOfProducts (Cons x y) = Right (x,y)

ignoreHelp :: Object a => Either (Static ()) (Static (Reference (ListI a), a)) ->. Token ->. Token
ignoreHelp (Left s) t = L.ignore s t
ignoreHelp (Right s) t = C.uncurry (\x y -> L.ignore x (L.ignore y t)) (C.unzip s)

-- instance Object f => Referential (List f) where
--   inhume t0 (Nil t1) = Nil (t0 <>. t1)
--   inhume t0 (Cons ref x) = Cons (L.inhume t0 ref) x
--   exhume (Nil t0) = C.second (\t1 -> Nil t1) (C.coappend t0)
--   exhume (Cons ref x) = C.second (\r -> Cons r x) (L.exhume ref)

-- | The empty list
nil ::
      Token 
  ->. List a
nil t = List t Nil

-- | /O(1)/ Push a new element onto the head of the list.
cons :: Object a
  =>  a
  ->. List a
  ->. List a
cons x (List t0 xs) = C.uncurry
  (\t1 r -> List t1 (Cons r x))
  (R.allocate xs t0)

-- | /O(1)/ Pop the element off of the head of the list.
uncons :: Object a
  =>  List a
  ->. Either Token (a, List a)
uncons (List t0 Nil) = Left t0
uncons (List t0 (Cons ref x)) = Right
  ( x
  , C.uncurry (\t1 ys -> List (t0 <>. t1) ys) (R.deallocate ref)
  )

replicate :: Object a
  =>  Int
  ->  a
  ->  Token
  ->. List a
replicate n a t = replicateGo n a (nil t)

replicateGo :: Object a
  =>  Int
  ->  a
  ->  List a
  ->. List a
replicateGo n a xs = if n > 0
  then replicateGo (n - 1) a (cons a xs)
  else xs 


-- | /O(n)/ Append two lists.
append :: Object a => List a ->. List a ->. List a
-- There is a better way to do this that does not have an
-- Object constraint on the element type. However, it
-- uses an unsafe trick.
append (List t0 Nil) (List t1 ys) = List (t0 <>. t1) ys
append (List t0 (Cons xsRef x)) (List t1 ys) = List t0
  ( Cons
    (R.statically_' xsRef (\xs -> appendGo ys xs t1))
    x
  )

appendGo :: Object a
  =>  ListI a -- list to append to the end
  ->. Static (Reference (ListI a))
  ->. Token
  ->. Token
appendGo ys r t0 = appendGo2 (R.dynamically r (appendChoose ys)) t0

-- The bool is True if we should continue.
appendChoose :: Object a
  =>  ListI a
  ->. ListI a
  ->. Token
  ->. (Token, (ListI a,Maybe (ListI a)))
appendChoose ys Nil t = (t,(ys,Nothing))
appendChoose ys (Cons rs r) t = (t,(Cons rs r, Just ys))

appendGo2 :: Object a
  =>  (Static (Reference (ListI a)), Maybe (ListI a))
  ->. Token
  ->. Token
appendGo2 (ref,Nothing) !t0 = L.ignore ref t
appendGo2 (ref,Just ys) !t0 = C.uncurry
  (\t1 s -> appendGo3 (C.map sumOfProducts s) (t0 <>. t1))
  (R.read ref)

appendGo3 :: Object a
  =>  Either (Static ()) (Static (Reference (ListI a), a))
  ->. Token
  ->. Token
appendGo3 

-- | Map over a list, updating the elements in-place.
map :: Object a
  =>  (a ->. a)
  ->  List a
  ->. List a
map g (List t0 Nil) = List t0 Nil
map g (List t0 (Cons ref x)) = C.uncurry
  (\t1 t2 -> List t1
    ( Cons 
      ( R.statically_
        ref
        (\xs -> mapHelp2 g xs t2)
      ) (g x)
    )
  ) (C.coappend t0)

-- | Map over a reference to a list, updating the elements in-place.
map' :: forall a. Object a
  =>  (a ->. a)
  ->  Static (Reference (ListI a))
  ->. Token
  ->. Token
map' g ref t0 = 
  C.uncurry (\t1 xs -> mapHelp2 g xs (t0 <>. t1)) (R.read (R.dynamically_ ref (mapHelp1 g)))

mapHelp2 :: Object a
  =>  (a ->. a)
  ->  Static (ListI a)
  ->. Token
  ->. Token
mapHelp2 g s t = mapHelp3 g (C.decide (C.map sumOfProducts s)) t

mapHelp3 :: Object a
  =>  (a ->. a)
  ->  Either (Static ()) (Static (Reference (ListI a), a))
  ->. Token
  ->. Token
mapHelp3 g (Left s) t = L.ignore s t
mapHelp3 g (Right s) t = C.uncurry
  (\xs a -> map' g xs (L.ignore a t))
  (C.unzip s)


mapHelp1 :: Object a
  =>  (a ->. a)
  ->  ListI a
  ->. ListI a
mapHelp1 g Nil = Nil
mapHelp1 g (Cons xs a) = Cons xs (g a)


-- I need to hook this up to a rewrite rule for map. Its just
-- a more efficient version of map specialized to a reference
-- element type. It is able to be more efficient because we
-- know not to overwrite the cons cells in the map, since the
-- references stay the same.
mapReference :: Object a
  =>  (a ->. a)
  ->  List (Reference a)
  ->. List (Reference a)
mapReference g (List t0 Nil) = List t0 Nil
mapReference g (List t0 (Cons xsRefD xRef)) = C.uncurry
  (\t1 t2 -> List t1
    ( Cons
      (R.statically_ xsRefD (\xs -> mapReferenceGo g (C.decide (C.map sumOfProducts xs)) t2))
      (R.modify_ xRef g)
    )
  ) (C.coappend t0)

mapReferenceGo :: Object a
  =>  (a ->. a)
  ->  Either (Static ()) (Static (Reference (ListI (Reference a)), Reference a))
  ->. Token
  ->. Token
mapReferenceGo g (Left s) !t0 = L.ignore s t0
mapReferenceGo g (Right s) !t0 = C.uncurry
  (\xsRef xRef -> C.uncurry
    (\t1 xsNext -> mapReferenceGo g (C.decide (C.map sumOfProducts xsNext)) (t1 <>. (L.ignore (R.dynamically_ xRef g) t0)))
    (R.read xsRef)
  ) (C.unzip s)

-- deallocate :: Object a
--   =>  List a
--   ->. Token
--   ->. Token
-- deallocate Nil t = t
-- deallocate (Cons xs x) = 

-- | A strict left fold over the list that deallocates the list as it goes.
foldl :: Object a
  =>  (b ->. a ->. b)
  ->  b
  ->. List a
  ->. (Token,b)
foldl g acc0 (List t0 xs) = foldlGo g acc0 xs t0

foldlGo :: Object a
  =>  (b ->. a ->. b)
  ->  b
  ->. ListI a
  ->. Token
  ->. (Token,b)
foldlGo g !acc Nil !t0 = (t0,acc)
foldlGo g !acc (Cons xs x) !t0 = C.uncurry (\t1 ys -> foldlGo g (g acc x) ys (t0 <>. t1)) (R.deallocate xs)

wordSize :: Int
wordSize = PM.sizeOf (undefined :: Addr)

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a

