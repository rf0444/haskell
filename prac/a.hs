import Data.List

-- {{{ fizzbuzz
fizzbuzz = map f [1..]
 where
  f x = case (mod x 3, mod x 5) of
   (0,0) -> "FizzBuzz"
   (0,_) -> "Fizz"
   (_,0) -> "Buzz"
   _ -> show x
-- where
--  f x = g (mod x 3) (mod x 5) x
--  g 0 0 _ = "FizzBuzz"
--  g 0 _ _ = "Fizz"
--  g _ 0 _ = "Buzz"
--  g _ _ x = show x
-- }}}
-- {{{ zipWithN, inversionStr
zipWithN f = map (foldl1 f) . transpose

inversionStr = unlines . transpose . fill . lines
 where
  fill xss = addBranks (maxLength xss) xss
--  maxLength = foldl (\a b -> max a $ length b) 0
  maxLength = maximum . map length
--  addBranks n [] = []
--  addBranks n (xs:xss) = (addBrank n xs) : (addBranks n xss)
  addBranks n = map (addBrank n)
  addBrank n xs = xs ++ (brank $ n - length xs)
--  brank 0 = []
--  brank n = ' ':(brank $ n - 1)
  brank n = take n $ repeat ' '
-- }}}
-- {{{ primes, fibs
primes = f [2] [2..]
 where
  f (x:xs) ys = ps ++ f (xs ++ ps) [z | z <- qs, mod z x /= 0]
   where
    (ps, qs) = span (< x^2) ys

fibs@(_:fibs') = 0:1:zipWith (+) fibs fibs'
-- }}}
-- {{{ revs
myRev1 [] = []
myRev1 (x:xs) = myRev1 xs ++ [x]

myRev2 xs = myRev2' xs []
 where
  myRev2' [] ys = ys
  myRev2' (x:xs) ys = myRev2' xs (x:ys)

myRev3 = foldl (flip (:)) []
-- }}}
-- {{{ fold
myFoldL :: (a -> b -> a) -> a -> [b] -> a
myFoldL f y xs = foldr (\x g a -> g $ f a x) id xs y

myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR f y xs = foldl (\g x a -> g $ f x a) id xs y

-- 間違い : 適用が右から
myFoldLb :: (a -> b -> a) -> a -> [b] -> a
myFoldLb f y xs = foldr (\x g a -> f (g a) x) id xs y
-- 間違い : 適用が左から
myFoldRb :: (a -> b -> b) -> b -> [a] -> b
myFoldRb f y xs = foldl (\g x a -> f x $ g a) id xs y
-- }}}

