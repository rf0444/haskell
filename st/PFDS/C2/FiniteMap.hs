{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances #-}

-- 2.6
module PFDS.C2.FiniteMap where
import Prelude hiding (lookup)
import PFDS.C2.Set
import Control.Monad

class FiniteMap m k where
  empty :: m k a
  bind :: k -> a -> m k a -> m k a
  lookup :: k -> m k a -> Maybe a

-- 設計的になんか違う気が・・・

data TreeMapEntry k a = Entry k a
instance (Eq k) => Eq (TreeMapEntry k a) where
  (Entry a _) == (Entry b _) = a == b
instance (Ord k) => Ord (TreeMapEntry k a) where
  compare (Entry a _) (Entry b _) = compare a b
instance (Show k, Show a) => Show (TreeMapEntry k a) where
  show (Entry k x) = "(" ++ show k ++ " -> " ++ show x ++ ")"

data TreeFiniteMap k a = Finite (Tree (TreeMapEntry k a))
 deriving Show
instance (Ord k) => FiniteMap TreeFiniteMap k where
  empty = Finite Emp
  bind k x (Finite Emp) = Finite (Node Emp (Entry k x) Emp)
  bind k1 x (Finite (Node l y@(Entry k2 v) r)) = f (compare k1 k2)
   where
    f EQ = (Finite (Node l (Entry k1 x) r))
    f LT = (Finite (Node (bind' l) y r))
    f GT = (Finite (Node l y (bind' r)))
    bind' t = let (Finite y) = bind k1 x (Finite t) in y
  lookup _ (Finite Emp) = Nothing
  lookup x (Finite (Node l (Entry k v) r))
   | x == k = Just v
   | otherwise = lookup x (Finite l) `mplus` lookup x (Finite r)

