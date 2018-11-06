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
import Linear.Types (Object,Referential,Reference,Mode,PrimObject(..),Mode(..),Token,Action)
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
  ignore Nil !t0 = t0
  ignore (Cons ref a) !t = L.ignore ref (L.ignore a t)

-- instance Object f => Referential (List f) where
--   inhume t0 (Nil t1) = Nil (t0 <>. t1)
--   inhume t0 (Cons ref x) = Cons (L.inhume t0 ref) x
--   exhume (Nil t0) = C.second (\t1 -> Nil t1) (C.coappend t0)
--   exhume (Cons ref x) = C.second (\r -> Cons r x) (L.exhume ref)

-- | /O(1)/ Push a new element onto the head of the list.
cons :: Object a
  => a
  -> List a
  -> List a
cons x (List t0 xs) =
  uncurry List
    ( R.dynamically_'
      ( \c0 -> C.second (\r -> Cons r x) (R.allocate xs c0)
      ) t0
    )

uncons :: Object a
  =>  List a
  ->. Either Token (a, List a)
uncons (List t0 Nil) = Left t0
uncons (List t0 (Cons ref x)) = Right
  ( x
  , C.uncurry List (R.dynamically_' (R.deallocate ref) t0)
  )

-- uncons (Nil t0) = Left t0
-- uncons (Cons ref x) = C.uncurry (\t0 xs -> Right (x,L.inhume t0 xs)) (R.deallocate ref)

-- unconsHelp :: Reference (ListI a) ->. Action 'Dynamic ->. ListI a
-- unconsHelp = R.deallocate

-- | Map over a list, updating the elements in-place.
map :: Object a
  =>  (a ->. a)
  ->  List a
  ->. List a
map g (List t0 Nil) = List t0 Nil
map g (List t0 (Cons ref x)) = List t0
  ( Cons 
    ( R.statically_
      ref
      (\xs t -> mapHelp2 g xs t)
    ) (g x)
  )

-- | Map over a reference to a list, updating the elements in-place.
map' :: forall a. Object a
  =>  (a ->. a)
  ->  Reference (ListI a)
  ->. Action 'Static
  ->. Action 'Static
map' g ref t0 = 
  C.uncurry (\t1 xs -> mapHelp2 g xs t1) (R.read (R.modify ref (mapHelp1 g)) t0)

mapHelp2 :: Object a
  =>  (a ->. a)
  ->  ListI a
  ->. Action 'Static
  ->. Action 'Static
mapHelp2 g Nil !t1 = t1
mapHelp2 g (Cons xs a) !t = map' g xs (L.ignore a t)

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
mapReference g (List t0 (Cons xsRefD xRef)) = List t0
  ( Cons
    (R.statically_ xsRefD (\xs t0 -> mapReferenceGo g xs t0))
    (R.modify xRef g)
  )

mapReferenceGo :: Object a
  =>  (a ->. a)
  ->  ListI (Reference a)
  ->. Action 'Static
  ->. Action 'Static
mapReferenceGo g Nil !t0 = t0
mapReferenceGo g (Cons xsRef xRef) !t0 = 
  C.uncurry (\t1 xsNext -> mapReferenceGo g xsNext (L.ignore (R.modify xRef g) t1)) (R.read xsRef t0)

wordSize :: Int
wordSize = PM.sizeOf (undefined :: Addr)

infixr 0 $.
($.) :: (a ->. b) ->. a ->. b
f $. a = f a

