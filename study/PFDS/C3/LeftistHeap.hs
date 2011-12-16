{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances #-}

module PFDS.C3.LeftistHeap where
import PFDS.Heap

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)
instance (Show a) => Show (LeftistHeap a) where
  show E = "(empty)"
  show (T r x a b) = "(" ++ show a ++ " : " ++ show x ++ " - " ++ show r ++ " : " ++ show b ++ ")"

instance (Ord a) => Heap LeftistHeap a where
  empty = E
  isEmpty E = True
  isEmpty _ = False
  -- 3.2
  --insert x h = merge (T 1 x E E) h
  insert x E = T 1 x E E
  insert x h@(T _ y a b)
   | x <= y = makeT x E h
   | otherwise = makeT y a (insert x b)
  merge E h = h
  merge h E = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
   | x <= y = makeT x a1 (merge b1 h2)
   | otherwise = makeT y a2 (merge h1 b2)
  findMin E = Nothing
  findMin (T _ x _ _) = Just x
  deleteMin E = E
  deleteMin (T _ _ a b) = merge a b

rank E = 0
rank (T r _ _ _) = r

makeT x a b 
 | rank a >= rank b = T (rank b + 1) x a b
 | otherwise = T (rank a + 1) x b a

-- 3.3
fromList :: (Ord a) => [a] -> LeftistHeap a
fromList = foldr merge empty . map (\x -> insert x E)
--fromList xs = 
-- where
--  eles = map (\x -> T 1 x E E) xs

