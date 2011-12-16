import Test.HUnit

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq)

instance Show a => Show (Tree a) where
  show Leaf = "(leaf)"
  show (Node l v r) = "(node: " ++ show v ++ " : " ++ show l ++ ", " ++ show r ++ ")"

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Node l v r) = Node (treeMap f l) (f v) (treeMap f r)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

-- test

test0, test1, test2, test4 :: Tree Int
test0 = Leaf
test1 = Node Leaf 1 Leaf
test2 = Node (Node Leaf 1 Leaf) 2 Leaf
test4 = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)

treeMapTests :: [Test]
treeMapTests = map TestCase
  [ assertEqual "treeMap (+ 1) test0"
                Leaf
                (treeMap (+ 1) test0)
  , assertEqual "treeMap (+ 1) test1"
                (Node Leaf 2 Leaf)
                (treeMap (+ 1) test1)
  , assertEqual "treeMap (+ 1) test2"
                (Node (Node Leaf 2 Leaf) 3 Leaf)
                (treeMap (+ 1) test2)
  , assertEqual "treeMap (+ 1) test4"
                (Node (Node (Node Leaf 2 Leaf) 3 Leaf) 4 (Node Leaf 5 Leaf))
                (treeMap (+ 1) test4)
  , assertEqual "treeMap show test0"
                Leaf
                (treeMap show test0)
  , assertEqual "treeMap show test1"
                (Node Leaf "1" Leaf)
                (treeMap show test1)
  , assertEqual "treeMap show test2"
                (Node (Node Leaf "1" Leaf) "2" Leaf)
                (treeMap show test2)
  , assertEqual "treeMap show test4"
                (Node (Node (Node Leaf "1" Leaf) "2" Leaf)
                      "3"
                      (Node Leaf "4" Leaf))
                (treeMap show test4)
  ]

fmapTests :: [Test]
fmapTests = map TestCase
  [ assertEqual "fmap (+ 1) test0"
                Leaf
                (fmap (+ 1) test0)
  , assertEqual "fmap (+ 1) test1"
                (Node Leaf 2 Leaf)
                (fmap (+ 1) test1)
  , assertEqual "fmap (+ 1) test2"
                (Node (Node Leaf 2 Leaf) 3 Leaf)
                (fmap (+ 1) test2)
  , assertEqual "fmap (+ 1) test4"
                (Node (Node (Node Leaf 2 Leaf) 3 Leaf) 4 (Node Leaf 5 Leaf))
                (fmap (+ 1) test4)
  , assertEqual "fmap show test0"
                Leaf
                (fmap show test0)
  , assertEqual "fmap show test1"
                (Node Leaf "1" Leaf)
                (fmap show test1)
  , assertEqual "fmap show test2"
                (Node (Node Leaf "1" Leaf) "2" Leaf)
                (fmap show test2)
  , assertEqual "fmap show test4"
                (Node (Node (Node Leaf "1" Leaf) "2" Leaf)
                      "3"
                      (Node Leaf "4" Leaf))
                (fmap show test4)
  ]

runTests :: [Test] -> IO Counts
runTests ts = runTestTT $ TestList ts

main = runTests $ treeMapTests ++ fmapTests
