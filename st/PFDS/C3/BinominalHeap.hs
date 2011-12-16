{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances #-}

module PFDS.C3.BinominalHeap where
import PFDS.Heap

data (Ord a) => BinominalHeapTree a = Node Int a [BinominalHeapTree a]
instance (Ord a, Show a) => Show (BinominalHeapTree a) where
  show (Node r x xs) = "(" ++ show x ++ " - " ++ show r ++ " : " ++ show xs ++")"

link :: (Ord a) => BinominalHeapTree a -> BinominalHeapTree a -> BinominalHeapTree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
 | x1 <= x2 = Node (r + 1) x1 (t2 : c1)
 | otherwise = Node (r + 1) x2 (t1 : c2)

data (Ord a) => BinominalHeap a = BH [BinominalHeapTree a]
instance (Ord a, Show a) => Show (BinominalHeap a) where
  show (BH ts) = show ts

rank :: (Ord a) => BinominalHeapTree a -> Int
rank (Node r _ _) = r

root :: (Ord a) => BinominalHeapTree a -> a
root (Node _ x _) = x

insTree :: (Ord a) => BinominalHeapTree a -> [BinominalHeapTree a] -> [BinominalHeapTree a]
insTree t [] = [t]
insTree t ts@(t':ts')
 | rank t < rank t' = t : ts
 | otherwise = insTree (link t t') ts'

instance (Ord a) => Heap BinominalHeap a where
  empty = BH []
  isEmpty (BH ts) = null ts
  merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)
  insert x (BH ts) = BH (insTree (Node 0 x []) ts)
  findMin (BH ts) = fmap (root . fst) (removeMinTree ts)
  deleteMin (BH ts) = BH (maybe [] f (removeMinTree ts))
   where
    f (Node _ x ts1, ts2) = mrg (reverse ts1) ts2

mrg :: (Ord a) => [BinominalHeapTree a] -> [BinominalHeapTree a] -> [BinominalHeapTree a]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2') = mrg' (compare (rank t1) (rank t2))
 where
  mrg' LT = t1 : mrg ts1' ts2
  mrg' GT = t2 : mrg ts1 ts2'
  mrg' EQ = insTree (link t1 t2) (mrg ts1' ts2')

removeMinTree :: (Ord a) => [BinominalHeapTree a] -> Maybe (BinominalHeapTree a, [BinominalHeapTree a])
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts) = fmap f (removeMinTree ts)
 where
  f (t', ts')
   | root t <= root t' = (t, ts)
   | otherwise = (t', t:ts')

