{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances #-}

module PFDS.Heap where

class (Ord a) => Heap h a where
  empty :: h a
  isEmpty :: h a -> Bool
  insert :: a -> h a -> h a
  merge :: h a -> h a -> h a
  findMin :: h a -> Maybe a
  deleteMin :: h a -> h a

