
module PFDS.C4.ISort where

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x ys@(y:ys') = f $ compare x y -- ← y をとってくる時に後ろの isort' が評価される
 where
  f GT = y : insert x ys' -- ← ここの insert が遅延
  f _ = x : ys

isort :: Ord a => [a] -> [a]
isort xs = isort' [] xs

isort' :: Ord a => [a] -> [a] -> [a]
isort' ys [] = ys
--isort' ys (x:xs) = isort' (insert x ys) xs
isort' ys (x:xs) = insert x (isort' ys xs)

