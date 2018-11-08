{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Linear.Reference
  ( -- * Primitives
    read
  , statically
  , statically_
  , statically_'
  , dynamically
  , dynamically_
  , modify
  , modify_
  , allocate
  -- , allocate'
  , deallocate
  , run
    -- * Reasoning
    -- $reasoning
  ) where

import Prelude hiding (read)

import Data.Primitive (Addr)
import Linear.Types (Object,Token)
import Linear.Unsafe (Reference(..),Static(..))
import Linear.Class (Unrestricted(..),(<>.))
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Linear.Unsafe as U
import qualified Linear.Types as L
import qualified Linear.Class as C
import qualified GHC.Exts as E

-- | Read the value at a static reference, discarding the reference.
read :: Object a
  =>  Static (Reference a)
  ->. (Token, Static a)
read (Static (Reference addr t0)) = C.second Static (L.peek addr t0)

statically_ :: Object a
  =>  Reference a
  ->. (Static a ->. Token)
  ->. Reference a
statically_ (Reference addr t0) f =
  Reference addr (C.uncurry (\t1 a -> t1 <>. f (Static a)) (L.peek addr t0))

statically_' :: 
      Reference a
  ->. (Static (Reference a) ->. Token)
  ->. Reference a
statically_' (Reference addr t0) f =
  Reference addr (f (Static (Reference addr t0)))

statically :: Object a
  =>  Reference a
  ->. (Static a ->. (Token, Unrestricted b))
  ->. (Reference a, Unrestricted b)
statically (Reference addr t0) f =
  C.uncurry (\t1 a -> C.uncurry (\t2 u -> (Reference addr (t1 <>. t2), u)) (f (Static a))) (L.peek addr t0)

-- statically ::
--       Reference a
--   ->. (a ->. Action 'Static ->. (Action 'Static, Unrestricted b))
--   ->. (Reference a, Unrestricted b)
-- statically (Reference addr t0) f = staticallyStep addr (f (Reference addr t0))
-- 
-- staticallyStep :: Addr -> (Action 'Static, Unrestricted b) ->. (Reference a, Unrestricted b)
-- staticallyStep !addr (!t1,u) = (Reference addr t1, u)

-- | Read a reference, operating on its object dynamically.
dynamically_ :: Object a
  =>  Static (Reference a)
  ->. (a ->. a)
  ->. Static (Reference a)
dynamically_ (Static (Reference addr t0)) f =
  (C.uncurry (\t1 a -> Static (Reference addr (L.poke addr (f a) t1))) (L.peek addr t0))

-- | Read a reference, operating on its object dynamically. Additionally,
--   return extra data. The extra data does not need to be unrestricted.
--
--   TODO: Think very carefully about whether or not this is actually safe.
dynamically :: Object a
  =>  Static (Reference a)
  ->. (a ->. Token ->. (Token, (a, b)))
  ->. (Static (Reference a), b)
dynamically (Static (Reference addr t0)) f =
  (C.uncurry (\t1 a -> C.uncurry (\t2 (a',b) -> (Static (Reference addr (L.poke addr a' t2)),b)) (f a t1)) (L.peek addr t0))

-- | Repeatedly follow the references until the callback returns @Left@.
--   At that point, update the reference we are currently in, and then
--   return.
dynamicallyUntil :: Object a
  =>  Static (Reference a)
  ->. (Static a ->. Either (Static b) (Static (Reference a)))
  ->  (b ->. a)
  ->. Token
dynamicallyUntil (Static (Reference addr t0)) f g = C.uncurry
  (\t1 a -> dynamicallyUntilGo addr (f a) f g t1)
  (L.peek addr t0)

dynamicallyUntilGo :: Object a
  =>  Addr -- the parent, that is, the address from which the Either came
  ->  Either b (Static (Reference a))
  ->. (Static a ->. Either b (Static (Reference a)))
  ->  (b ->. a)
  ->. Token
  ->. Token
dynamicallyUntilGo addr (Left b) f g t = L.poke addr (g b) t
dynamicallyUntilGo addr (Right (Static (Reference addrNext t))) f g =
  dynamicallyUntilGo addrNext _ f g _
  (L.peek addr t0)


-- -- | This function is truly terrible. Its implementation requires an unsafe
-- --   coercion of linearity. However, I cannot think of a implementation
-- --   that does not do this. This function is kind of like @dynamically@
-- --   followed by @read@ except that we are allowed to preserve knowledge
-- --   about how the reference was updated. Also, think hard about the
-- --   correctness of this function.
-- dynamically2 :: Object a
--   =>  Static (Reference a)
--   ->. (a ->. Token ->. (Token, Either (b,c) (d,e)))
--   ->. (b ->. a)
--   ->  (d ->. a)
--   ->  (Token, Either (Static b,c) (Static c,d))
-- dynamically2 (Static (Reference addr t0)) f g h = C.uncurry
--   (\t1 a -> C.uncurry (goDynamically2 g h) (f a t1))
--   (L.peek addr t0)
-- 
-- goDynamically2 ::
--       (b ->. a)
--   ->  (d ->. a)
--   ->  Token
--   ->. Either (b,c) (d,e)
--   ->. (Token, Either (Static b,c) (Static c,d))
-- goDynamically2 g h t0 (Left (b0,c)) = C.uncurry (\b1 b2 -> (_,Left (Static b2,c)) (dupUnsafe b0)

dupUnsafe :: a ->. (a,a)
dupUnsafe = unsafeCoerce dupNonlinear

dupNonlinear :: a -> (a,a)
dupNonlinear x = (x,x)

--   Use a neutral token as a dynamic action. The token is restored after
--   the continuation finishes. The only way that a dynamic action can
--   escape this function is if the type @a@ itself has dynamic actions.
--   In that case, it is fine. This function relies on the impossibility
--   of duplicating a dynamic action.
-- dynamically_' ::
--       (Action 'Dynamic ->. (Action 'Dynamic, a))
--   ->. Token
--   ->. (Token, a)
-- dynamically_' f (U.Token s0) =
--   C.first tokenize (f (U.Action s0))

-- | Allocate an object on the unmanaged heap.
allocate :: forall a. Object a
  =>  a
  ->. Token
  ->. (Token, Reference a)
allocate a !c0 = U.withAllocatedBytes (L.size (Proxy :: Proxy a)) c0
  (\addr c1 -> C.second (\c2 -> Reference addr (L.poke addr a c2)) (C.coappend c1))

-- Allocate a referential object on the unmanaged heap.
-- allocate' :: forall f. Referential a
--   =>  f 'Dynamic
--   ->. Reference f 'Dynamic
-- allocate' a = C.uncurry (C.flip allocate) (L.exhume a)

-- | Deallocate an object from the unmanaged heap.
deallocate :: forall a. Object a
  =>  Reference a
  ->. (Token, a)
deallocate (Reference addr t0) = C.uncurry
  (\t1 a -> U.withDeallocate addr t1 (\t2 -> (t2, a)))
  (L.peek addr t0)

-- | Modify a reference, returning some additional unrestricted data.
modify :: Object a
  =>  Reference a
  ->. (a ->. (a,b))
  ->. (Reference a,b)
modify (Reference addr t0) f =
  (C.uncurry (\t1 (a,u) -> (Reference addr (L.poke addr a t1), u)) (C.second f (L.peek addr t0)))

-- | Modify a reference.
modify_ :: Object a
  =>  Reference a
  ->. (a ->. a)
  ->. Reference a
modify_ (Reference addr t0) f =
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
 
This commentary is now out of date. Fix this.
 
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
