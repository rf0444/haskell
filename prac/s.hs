
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort(filter (<= x) xs) ++ (x : qsort(filter (x <) xs))

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort l = merge (msort a) (msort b) 
 where
  (a, b) = splitAt ((length l) `div` 2) l
  merge xs [] = xs
  merge [] ys = ys
  merge xs@(x:xs') ys@(y:ys')
   | x < y = x : merge xs' ys
   | otherwise = y : merge xs ys'

