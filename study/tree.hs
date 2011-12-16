
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show (Leaf val) = "(leaf : " ++ show val ++ ")"
  show (Node l r) = "(node : " ++ show l ++ ", " ++ show r ++ ")"

instance Monad Tree where
  return val = Leaf val
  (>>=) (Leaf val) f = f val
  (>>=) (Node l r) f = Node (l >>= f) (r >>= f)

f :: Int -> Int
f 0 = 10
f 3 = 99
f x = x + y + a
 where
  a = 45 + y
  y = 5 + x + a

main = display ele
 where
  display = putStrLn . show
  ele = Node (Leaf $ f 3) (Node (Leaf $ f 4) (Leaf $ f 5))

