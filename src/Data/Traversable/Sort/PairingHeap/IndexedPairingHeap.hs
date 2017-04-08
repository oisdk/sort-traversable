{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, GADTs, RoleAnnotations #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Data.Traversable.Sort.PairingHeap.IndexedPairingHeap (
    Heap
  , empty
  , singleton
  , insert
  , merge
  , minView
  ) where

import GHC.TypeLits

-- | Okasaki's simple representation of a pairing heap, but with
-- a size index.
data Heap n a where
  E :: Heap 0 a
  T :: a -> HVec n a -> Heap (1 + n) a

-- Coercing a heap could destroy the heap property, so we declare both
-- type parameters nominal.
type role Heap nominal nominal

-- | A vector of heaps whose sizes sum to the index.
data HVec n a where
  HNil :: HVec 0 a
  HCons :: Heap m a -> HVec n a -> HVec (m + n) a

-- Produce an empty heap
empty :: Heap 0 a
empty = E

-- Produce a heap with one element
singleton :: a -> Heap 1 a
singleton a = T a HNil

-- Insert an element into a heap
insert :: Ord a => a -> Heap n a -> Heap (1 + n) a
insert x xs = merge (singleton x) xs
{-# INLINABLE insert #-}

-- Merge two heaps
merge :: Ord a => Heap m a -> Heap n a -> Heap (m + n) a
merge E ys = ys
merge xs E = xs
merge h1@(T x xs) h2@(T y ys)
  | x <= y = T x (HCons h2 xs)
  | otherwise = T y (HCons h1 ys)
{-# INLINABLE merge #-}

-- Get the smallest element of a non-empty heap, and the rest of
-- the heap
minView :: Ord a => Heap (1 + n) a -> (a, Heap n a)
minView (T x hs) = (x, mergePairs hs)
{-# INLINABLE minView #-}

mergePairs :: Ord a => HVec n a -> Heap n a
mergePairs HNil = E
mergePairs (HCons h HNil) = h
mergePairs (HCons h1 (HCons h2 hs)) =
    merge (merge h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}
