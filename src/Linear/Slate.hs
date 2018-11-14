{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Linear.Slate
  ( Slate
    -- * Dynamic
  , allocate
  , resize
  , push
  , pop
  , insert
  , length
  , deallocateEmpty
    -- * Static
  , read
    -- * Consume
  , foldl
  ) where

import Prelude hiding (length,read,foldl)

import Data.Proxy (Proxy(..))
import Data.Primitive (Addr)
import Linear.Unsafe (Slate(..))
import Linear.Types (Mode(Static,Dynamic),Object,Token)
import Linear.Class ((<>.),Unrestricted(..))
import GHC.IO (IO(..))
import GHC.Exts (State#,RealWorld)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Primitive as PM
import qualified Linear.Class as C
import qualified Linear.Types as L
import qualified Linear.Unsafe as U

-- | Allocate an empty slate.
allocate :: forall f. Object f
  =>  Int -- ^ Maximum number of elements
  ->  Token
  ->. Slate f 'Dynamic
allocate n t0 = U.withAllocatedBytes
  (L.size (Proxy :: Proxy f) * n)
  t0
  (\addr t1 -> Slate addr 0 t1)

-- | Deallocate an empty slate. The argument slate must be
--   empty. Otherwise, this funcion has undefined behavior.
deallocateEmpty :: Object f
  =>  Slate f 'Dynamic
  ->. Token
deallocateEmpty (Slate addr sz t0) = U.withDeallocate addr t0 C.id

-- | The number of elements currently present in the slate.
length ::
      Slate f 'Dynamic
  ->. (Slate f 'Dynamic, Unrestricted Int)
length (Slate addr sz t0) = (Slate addr sz t0, Unrestricted sz)

-- | Push an element onto the right side of the slate. This
--   function has undefined behavior when the slate is already
--   full.
push :: forall f. Object f
  =>  Slate f 'Dynamic
  ->. f 'Dynamic
  ->. Slate f 'Dynamic
push (Slate addr sz t0) x = Slate addr (sz + 1)
  (L.poke (PM.plusAddr addr (sz * L.size (Proxy :: Proxy f))) (L.inhume t0 x))

-- | Pop an element off of the right side of the slate. This
--   function has undefined behavior when the slate is empty.
--   This can be used in both a dynamic and a static context.
--   This means that this function can be used by accomplish
--   something similar to the traditional @peek@ operation on
--   queues.
pop :: forall f m. Object f
  =>  Slate f m
  ->. (Slate f m, f m)
pop (Slate addr sz t0) =
  C.first (Slate addr (sz - 1)) (L.exhume (L.peek (PM.plusAddr addr ((sz - 1) * L.size (Proxy :: Proxy f))) t0))

-- | Insert an element into a slate at the given index. The index
--   must be between zero and the current size (inclusive bounds).
--   Providing an index outside of these bounds results in undefined
--   behavior.
insert :: forall f. Object f
  =>  Slate f 'Dynamic
  ->. Int
  ->  f 'Dynamic
  ->. Slate f 'Dynamic
insert (Slate addr sz t0) ix val = Slate addr (sz + 1)
  (moveAddr dst src bytes (L.poke src (L.inhume t0 val)))
  where
  elemSz = L.size (Proxy :: Proxy f)
  src = PM.plusAddr addr (elemSz * ix)
  dst = PM.plusAddr src elemSz
  bytes = (sz - ix) * elemSz

-- | Resize a slate. The new size must be larger than the
--   current number of elements or this function has undefined
--   behavior.
resize :: forall f. Object f
  =>  Slate f 'Dynamic
  ->. Int
  ->  Slate f 'Dynamic
resize (Slate addr0 sz t0) !n = U.withReallocatedBytes
  (L.size (Proxy :: Proxy f) * n)
  addr0
  t0
  (\addr1 t1 -> Slate addr1 sz t1)

-- | Read the value at the given position in the slate. Out of bounds
--   access results in undefined behavior. This function discards all
--   other values in the slate.
read :: forall f. Object f
  =>  Slate f 'Static
  ->. Int
  ->  f 'Static
read (Slate addr sz t0) !ix = L.peek
  (PM.plusAddr addr (ix * L.size (Proxy :: Proxy f)))
  t0

-- | Left fold over the slate, strict in the accumulator. This deallocates
--   the slate as it walks over it.
foldl :: Object f
  =>  (b ->. f 'Dynamic ->. b)
  ->  b
  ->. Slate f 'Dynamic
  ->. (Token,b)
foldl f !b0 (Slate addr sz t0) = foldlGo (sz > 0) f b0 (Slate addr sz t0)

foldlGo :: Object f
  =>  Bool
  ->. (b ->. f 'Dynamic ->. b)
  ->  b
  ->. Slate f 'Dynamic
  ->. (Token,b)
foldlGo False g !b0 s = (deallocateEmpty s, b0)
foldlGo True g !b0 s = C.uncurry
  ( \(Slate addr sz t1) val -> C.uncurry
    ( \t2 val' -> foldlGo (sz > 0) g (g b0 val') (Slate addr sz (t1 <>. t2))
    ) (L.exhumeAll val)
  ) (pop s)

moveAddr :: Addr -> Addr -> Int -> Token ->. Token
moveAddr = unsafeCoerce moveAddrNonlinear

moveAddrNonlinear :: Addr -> Addr -> Int -> Token -> Token
moveAddrNonlinear dst src bytes (U.Token s0) = U.Token (unIO_ (PM.moveAddr dst src bytes) s0)

unIO_ :: IO () -> State# RealWorld -> State# RealWorld
unIO_ (IO g) s = case g s of
  (# s', _ #) -> s'



