
data List a = Nil | Cons a (List a)
 deriving Show

push :: a -> List a -> List a
push x xs = Cons x xs

pop :: List a -> List a
pop Nil = Nil
pop (Cons _ xs) = xs

peek :: List a -> a
peek Nil = error "no element!"
peek (Cons x _) = x

llength :: List a -> Int
llength Nil = 0
llength (Cons _ xs) = 1 + llength xs

get :: Int -> List a -> a
get _ Nil = error "no element!"
get 0 (Cons x _) = x
get n (Cons _ xs) = get (n - 1) xs

set :: a -> Int -> List a -> List a
set _ _ Nil = error "no element!"
set y 0 (Cons _ xs) = Cons y xs
set y n (Cons x xs) = Cons x (set y (n - 1) xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

lmap :: (a -> b) -> List a -> List b
lmap _ Nil = Nil
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

lfilter :: (a -> Bool) -> List a -> List a
lfilter _ Nil = Nil
lfilter f (Cons x xs)
 | f x = Cons x remains
 | otherwise = remains
 where
  remains = lfilter f xs

sort :: Ord a => List a -> List a
sort Nil = Nil
sort (Cons x xs) = append (sort le) (Cons x (sort gt))
 where
   le = lfilter (<= x) xs
   gt = lfilter (x <) xs

lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr _ y Nil = y
lfoldr f y (Cons x xs) = f x (lfoldr f y xs)

lfoldl :: (b -> a -> b) -> b -> List a -> b
lfoldl _ y Nil = y
lfoldl f y (Cons x xs) = lfoldl f (f x y) xs


