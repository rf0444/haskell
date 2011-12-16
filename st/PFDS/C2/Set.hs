{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances #-}

-- 2.2 - 2.6
module PFDS.C2.Set where
import PFDS.Set

data Tree a = Emp | Node (Tree a) a (Tree a)
  deriving Show

instance (Ord a) => Set Tree a where
  empty = Emp
  -- 2.2
  member _ Emp = False
  member x (Node l v r) = f (compare x v)
   where
    f EQ = True
    f LT = member x l
    f GT = member x r
  -- 2.4
  insert x Emp = Node Emp x Emp
  insert x (Node l v r) = f (compare x v)
   where
    f EQ = Node l x r
    f LT = Node (insert x l) v r
    f GT = Node l v (insert x r)

-- 2.5
complete :: Ord a => a -> Int -> Tree a
complete _ 0 = Emp
complete x n = Node child x child
 where
  child = complete x (n - 1)

complete2 :: Ord a => a -> Int -> Tree a
complete2 x n = fst $ create2 x n
 where
  create2 x 0 = (Emp, Node Emp x Emp)
  create2 x n = f (mod (n - 1) 2)
   where
    f 0 = (Node sub x sub, Node sub1 x sub)
    f 1 = (Node sub1 x sub, Node sub1 x sub1)
    (sub, sub1) = create2 x (div (n - 1) 2)

size :: Ord a => Tree a -> Int
size Emp = 0
size (Node l _ r) = 1 + size l + size r

balanced :: Ord a => Tree a -> Bool
balanced Emp = True
balanced (Node l _ r) = abs (size l - size r) <= 1 && balanced l && balanced r

