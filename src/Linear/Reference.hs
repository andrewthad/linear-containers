{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Linear.Reference
  ( -- * Primitives
    ignore
  , read
  , statically
  , statically_
  , dynamically_
  , descend
  , modify
  , allocate
  , allocate'
  , deallocate
  , run
    -- * Reasoning
    -- $reasoning
  ) where

import Prelude hiding (read)

import Data.Kind (Type)
import Data.Primitive (Addr)
import Linear.Types (Mode(..),Object,Token,Referential(..))
import Linear.Unsafe (Reference(..))
import Linear.Class (Unrestricted(..))
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified GHC.Exts as E
import qualified Linear.Unsafe as U
import qualified Linear.Types as L
import qualified Linear.Class as C

-- | Discard a static reference.
ignore :: Reference f 'Static ->. Token
ignore (Reference _ t) = t

-- | Read the value at a static reference, discarding the reference.
read :: Object f
  =>  Reference f 'Static
  ->. (Token, f 'Static)
read (Reference addr t0) = L.peek addr t0

statically_ :: Object f
  =>  Reference f 'Dynamic
  ->. (Reference f 'Static ->. Token)
  ->. Reference f 'Dynamic
statically_ (Reference addr t0) f = Reference addr (f (Reference addr t0))

statically ::
      Reference f 'Dynamic
  ->. (Reference f 'Static ->. (Token, Unrestricted a))
  ->. (Reference f 'Dynamic, Unrestricted a)
statically (Reference addr t0) f = staticallyStep addr (f (Reference addr t0))

staticallyStep :: Addr -> (Token, Unrestricted a) ->. (Reference f 'Dynamic, Unrestricted a)
staticallyStep !addr (!t1,u) = (Reference addr t1, u)

-- | Read a static reference, operating on its object dynamically.
dynamically_ :: Object f
  =>  Reference f 'Static
  ->. (f 'Dynamic ->. f 'Dynamic)
  ->. Token
dynamically_ (Reference addr t0) f =
  C.uncurry (C.flip (L.poke addr)) (C.second f (L.peek addr t0))

-- | Repeatedly follow the references until the callback returns @Left@.
--   At that point, update the reference we are currently in, and then
--   return.
descend :: Object f
  =>  Reference f 'Static
  ->. (forall (m :: Mode). f m ->. Either (g m) (h m))
  ->  (g 'Static ->. Reference f 'Static)
  ->  (h 'Dynamic ->. f 'Dynamic)
  ->. Token
descend (Reference addr t0) = descendGo addr t0

descendGo :: Object f
  =>  Addr
  ->  Token
  ->. (forall (m :: Mode). f m ->. Either (g m) (h m))
  ->  (g 'Static ->. Reference f 'Static)
  ->  (h 'Dynamic ->. f 'Dynamic)
  ->.  Token
descendGo addr t0 f g h = L.peeking addr t0
  ( \t1 x -> descendEither addr t1 (f x) f g h
  ) 

descendEither :: Object f
  =>  Addr
  ->  Token
  ->. (forall m. Either (g m) (h m))
  ->. (forall (m :: Mode). f m ->. Either (g m) (h m))
  ->  (g 'Static ->. Reference f 'Static)
  ->  (h 'Dynamic ->. f 'Dynamic)
  ->.  Token
descendEither addr t0 (Left x) f g h = descend (L.inhume t0 (g x)) f g h
descendEither addr t0 (Right x) f g h = L.poke addr (h x) t0

-- descendEither :: Object f
--   =>  g 'Static
--   ->. h 'Static 

-- | Allocate an object on the unmanaged heap.
allocate :: forall f. Object f
  =>  f 'Dynamic
  ->. Token
  ->. Reference f 'Dynamic
allocate a !t0 = U.withAllocatedBytes (L.size (Proxy :: Proxy f)) t0
  (\addr t1 -> Reference addr (L.poke addr a t1))

-- | Allocate a referential object on the unmanaged heap.
allocate' :: forall f. Referential f
  =>  f 'Dynamic
  ->. Reference f 'Dynamic
allocate' a = C.uncurry (C.flip allocate) (L.exhume a)

-- | Deallocate an object from the unmanaged heap.
deallocate :: forall f. Object f
  =>  Reference f 'Dynamic
  ->. (Token, f 'Dynamic)
deallocate (Reference addr t0) = C.uncurry
  (\t1 a -> U.withDeallocate addr t1 (\t2 -> (t2, a)))
  (L.peek addr t0)

-- | Modify either a static reference or a dynamic reference.
modify :: Object f
  =>  Reference f m
  ->. (f 'Dynamic ->. f 'Dynamic)
  ->. Reference f m
modify (Reference addr t0) f =
  Reference addr (C.uncurry (C.flip (L.poke addr)) (C.second f (L.peek addr t0)))

run :: (Token ->. (Token,Unrestricted a)) ->. Unrestricted a
run = unsafeCoerce runNonlinear

runNonlinear :: (Token ->. (Token,Unrestricted a)) -> Unrestricted a
runNonlinear f = E.runRW#
  (\s -> case f (U.Token s) of
    (U.Token s',Unrestricted u) -> case E.seq# u s' of
      (# _, u' #) -> Unrestricted u'
  )

{- $reasoning
 
This API for dealing with references looks unusual. Provided here is an
explanation of how the author arrived at this API and an informal argument
for its soundness. When dealing with references, it is not immidiately
clear why one would want to distinguish between references that are
in different modes. Here is the naive API that first comes to mind:

> data Reference a
> data Token
> class Object a
>
> allocate :: Object a => a ->. Token ->. Reference a
> deallocate :: Reference a ->. (Token, a)
> modify :: Reference a ->. (a ->. (a,Unrestricted b)) ->. (Reference a, Unrestricted b)
> modify_ :: Reference a ->. (a ->. a) ->. Reference a

That is all. There are four simple functions, each of which has a
clear purpose. The last function, @modify_@, is not truly primitive
since it could be defined using @modify@. Internally, a reference
is equipped with a @State# RealWorld@ to ensure proper sequencing.
The use of tokens in the API is unfortunate, but there are objects
that do not contain references. In these cases, the token plays an
essential role in sequencing the computations appropriately.

Although this API allows us to write a lot of interesting data structures
and in-place algorithms, it does not always let us do so efficiently.
Consider a lookup function for a key-value map:

> data Map k v
> lookup :: k -> Map k v ->. (Map k v, Unrestricted (Maybe v))

The implementation of lookup should just read from the @Map@. But,
we cannot do that. While @modify@ is expressive enough to write
this function, it does so in a way that needlessly overwrites the
parent nodes after finding the value associted with the @Map@.
For a more complicated example, consider mapping over the elements of
a singlely-linked list:

> data List a = Cons a (Reference (List a)) | Nil
> instance Object a => Object (List a)
>
> map :: (a ->. a) -> List a ->. List a
> map f Nil = Nil
> map f (Cons x xs) = Cons (f x) (modify (map f) xs)

Semantically, this is correct. However, it does not perform as well as it
could. Notice that it is not tail recursive. Every call to @modify@ will
push a context onto the stack, and when we finally hit @Nil@, we must
walk all the way back down the stack. On the way back down, we
are simply overwriting the reference inside each @Cons@ with a reference
that is guaranteed to be identical. This guarantee of equality means that
we do not actually want the result from the recursive call to map. What we really
want @map@ to do is walk the list tail-recursively, modifying each element
as we go. When we hit @Nil@, we want to jump all the way back to
the front of the list and return it.

The cost an API that admits tail-recursion is additional complexity.
To see this API, just look at the functions provided by this module.
It accomplishes this by providing a way to @read@ or @ignore@ a reference
without giving the user back a new reference. These \"read-mode\" functions
must be scoped by @statically@, which recovers the original reference
after exectuting a computation that can freely discard references.
With these functions in hand, it\'s easy to see how the @lookup@ function
for the key-value @Map@ discussed above could be implemented more
efficiently.

With @dynamically@, we can go the other direction as well. This
allows us to take a static reference, cast its object to dynamic, execute
a computation on it, and finally discard the static reference we started
with. We are allowed the following actions by mode:

* Static: read and ignore
* Dynamic: allocate and deallocate

Note that both @statically@ and @dynamically@ read the reference
and execute a continuation on the object. So, to switch modes,
the user must follow a reference.

-}
