-- 2.1
module PFDS.C2.List where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs'@(_:xs) = xs' : suffixes xs

