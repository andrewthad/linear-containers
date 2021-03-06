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
  , burrow
  , modify
  , allocate
  , deallocate
  , run
    -- * Reasoning
    -- $reasoning
  ) where

import Prelude hiding (read)

import Data.Kind (Type)
import Data.Primitive (Addr)
import Linear.Types (Mode(..),Object,Token)
import Linear.Unsafe (Reference(..))
import Linear.Class (Unrestricted(..),(<>.))
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
  ->. f 'Static
read (Reference addr t0) = L.peek addr t0

-- | Statically scope a computation without producing an unrestricted
--   result.
statically_ :: Object f
  =>  Reference f 'Dynamic
  ->. (Reference f 'Static ->. Token)
  ->. Reference f 'Dynamic
statically_ (Reference addr t0) f = Reference addr (f (Reference addr t0))

-- | Statically scope a computation. This computation may freely
--   read from or ignore references to compute an unrestricted
--   result. However, it still may not duplicate a reference.
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
  -- TODO: Fix this. Currently, poke is not guaranteed to force all
  -- peeks to happen. I'm working on this.
  L.poke addr (f (L.peek addr t0))

-- | Repeatly follow references, performing modifications at each
--   step, until the callback returns @Left@.
burrow :: Object f
  =>  Reference f 'Static
  ->. (f 'Dynamic ->. Either (g 'Dynamic) (h 'Dynamic))
  ->  (C.LensLike ((,) (Reference f 'Static)) (g 'Dynamic) (f 'Dynamic) (Reference f 'Dynamic) (Reference f 'Dynamic)) -- ^ recursive step
  ->  (h 'Dynamic ->. f 'Dynamic) -- ^ final
  ->. Token
burrow (Reference addr t0) = burrowGo addr t0

burrowGo :: Object f
  =>  Addr
  ->  Token
  ->. (f 'Dynamic ->. Either (g 'Dynamic) (h 'Dynamic))
  ->  (C.LensLike ((,) (Reference f 'Static)) (g 'Dynamic) (f 'Dynamic) (Reference f 'Dynamic) (Reference f 'Dynamic)) -- ^ recursive step
  ->  (h 'Dynamic ->. f 'Dynamic) -- ^ final
  ->. Token
burrowGo addr t0 f g h = burrowEither addr (f (L.peek addr t0)) f g h

burrowEither :: Object f
  =>  Addr
  ->  Either (g 'Dynamic) (h 'Dynamic)
  ->. (f 'Dynamic ->. Either (g 'Dynamic) (h 'Dynamic))
  ->  (C.LensLike ((,) (Reference f 'Static)) (g 'Dynamic) (f 'Dynamic) (Reference f 'Dynamic) (Reference f 'Dynamic)) -- ^ recursive step
  ->  (h 'Dynamic ->. f 'Dynamic) -- ^ final
  ->. Token
burrowEither !addr (Right x) f g h = L.poke addr (h x)
burrowEither !addr (Left x) f g h = C.uncurry
  (\ref v -> burrow (L.inhume (L.poke addr v) ref) f g h)
  (g unsafeDuplicateReference x)

-- This does not require unsafeCoerce to write, but in general
-- this function is unsafe to use. Its use in burrowEither is safe.
unsafeDuplicateReference :: Reference f 'Dynamic ->. (Reference f 'Static, Reference f 'Dynamic)
unsafeDuplicateReference (Reference addr t0) = C.uncurry
  (\t1 t2 -> (Reference addr t1, Reference addr t2))
  (C.coappend t0)

-- | Repeatedly follow the references until the callback returns @Left@.
--   At that point, update the reference we are currently in, and then
--   return.
descend :: Object f
  =>  Reference f 'Static
  ->. (forall (m :: Mode). f m ->. Either (g m) (h m))
  ->  (g 'Static ->. Reference f 'Static) -- ^ recursive step
  ->  (h 'Dynamic ->. f 'Dynamic) -- ^ final
  ->. Token
descend (Reference addr t0) = descendGo addr t0

descendGo :: Object f
  =>  Addr
  ->  Token
  ->. (forall (m :: Mode). f m ->. Either (g m) (h m))
  ->  (g 'Static ->. Reference f 'Static)
  ->  (h 'Dynamic ->. f 'Dynamic)
  ->.  Token
descendGo addr t0 f g h = descendEither addr (f (L.peek addr t0)) f g h

descendEither :: Object f
  =>  Addr
  ->  (forall m. Either (g m) (h m))
  ->. (forall (m :: Mode). f m ->. Either (g m) (h m))
  ->  (g 'Static ->. Reference f 'Static)
  ->  (h 'Dynamic ->. f 'Dynamic)
  ->.  Token
descendEither !addr (Left x) f g h = descend (g x) f g h
descendEither !addr (Right x) f g h = L.poke addr (h x)

-- descendEither :: Object f
--   =>  g 'Static
--   ->. h 'Static 

-- | Allocate an object on the unmanaged heap.
allocate :: forall f. Object f
  =>  f 'Dynamic
  ->. Reference f 'Dynamic
allocate a = C.uncurry
  (\t0 a' ->
    U.withAllocatedBytes (L.size (Proxy :: Proxy f)) t0
    (\addr t1 -> Reference addr (t1 <>. L.poke addr a'))
  ) (L.exhume a)

-- | Deallocate an object from the unmanaged heap.
deallocate :: forall f. Object f
  =>  Reference f 'Dynamic
  ->. f 'Dynamic
deallocate (Reference addr t0) = C.uncurry
  (\t1 a -> U.withDeallocate addr t1 (\t2 -> L.inhume t2 a))
  (L.exhume (L.peek addr t0))

-- | Modify either a static reference or a dynamic reference.
modify :: Object f
  =>  Reference f m
  ->. (f 'Dynamic ->. f 'Dynamic)
  ->. Reference f m
modify (Reference addr t0) f =
  Reference addr (L.poke addr (f (L.peek addr t0)))

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
