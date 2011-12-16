{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances #-}

module PFDS.C3.RBT where
import PFDS.Set

data Color = R | B
 deriving Show

data Tree a = E | T Color (Tree a) a (Tree a)
instance (Show a) => Show (Tree a) where
  show E = "(empty)"
  show (T c a x b) = "(" ++ show a ++ " : " ++ show x ++ " - " ++ show c ++ " : " ++ show b ++ ")"

instance (Ord a) => Set Tree a where
  empty = E
  member x E = False
  member x (T _ a y b) = f (compare x y)
   where
    f LT = member x a
    f GT = member x b
    f EQ = True
  insert x s = T B a y b
   where
    ins E = T R E x E
    ins s@(T c a y b) = f (compare x y)
     where
      f LT = balance c (ins a) y b
      f GT = balance c a y (ins b)
      f EQ = s
    (T _ a y b) = ins s

balance :: (Ord a) => Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c a x b = T c a x b

