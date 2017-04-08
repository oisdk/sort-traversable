{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators,
      RankNTypes, InstanceSigs, DataKinds #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Traversable.Sort.PairingHeap where
import Data.Traversable.Sort.PairingHeap.IndexedPairingHeap (
    Heap
  , empty
  , singleton
  , merge
  , minView
  )

import GHC.TypeLits

-- | A heap of some size whose element have type @a@ and a
-- function that, applied to any heap at least that large,
-- will produce a result and the rest of the heap.
data Sort a r where
  Sort :: (forall n. Heap (m + n) a -> (Heap n a, r))
       -> !(Heap m a)
       -> Sort a r

instance Functor (Sort x) where
  fmap f (Sort g h) =
    Sort (\h' -> case g h' of (remn, r) -> (remn, f r)) h
  {-# INLINE fmap #-}

instance Ord x => Applicative (Sort x) where
  pure x = Sort (\h -> (h, x)) empty
  {-# INLINE pure #-}

  -- Combine two 'Sort's by merging their heaps and composing
  -- their functions.
  (<*>) :: forall a b . Sort x (a -> b) -> Sort x a -> Sort x b
  Sort f (xs :: Heap m x) <*> Sort g (ys :: Heap n x) =
    Sort h (merge xs ys)
    where
      h :: forall o . Heap ((m + n) + o) x -> (Heap o x, b)
      h v = case f v of { (v', a) ->
                case g v' of { (v'', b) ->
                  (v'', a b)}}
  {-# INLINABLE (<*>) #-}

-- Produce a 'Sort' with a singleton heap and a function that will
-- produce the smallest element of a heap.
liftSort :: Ord x => x -> Sort x x
liftSort a = Sort (\h -> case minView h of (x, h') -> (h', x)) (singleton a)
{-# INLINABLE liftSort #-}

-- Apply the function in a 'Sort' to the heap within, producing a
-- result.
runSort :: forall x a . Sort x a -> a
runSort (Sort (f :: Heap (m + 0) x -> (Heap 0 x, a)) xs) = snd $ f xs

-- | Sort an arbitrary 'Traversable' container using a heap.
sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable = runSort . traverse liftSort
{-# INLINABLE sortTraversable #-}

-- | Sort an arbitrary container using a 'Traversal' (in the
-- 'lens' sense).
sortTraversal :: Ord a => ((a -> Sort a a) -> t -> Sort a t) -> t -> t
sortTraversal trav = runSort . trav liftSort
{-# INLINABLE sortTraversal #-}
