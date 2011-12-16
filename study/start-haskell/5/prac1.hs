import Test.HUnit

data Tree = Leaf Int | Node Tree Tree

-- funcs

size :: Tree -> Int
size (Leaf _) = 1
size (Node l r) = size l + size r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = balanced l && balanced r && balanced'
 where
  balanced' = (<= 1) $ abs $ size l - size r

-- tests
t0, t1, t2, t3, t4 :: Tree
t0 = Leaf 1
t1 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
t2 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
t3 = Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4))
t4 = Node (Node (Node (Node (Leaf 1) (Leaf 2))
                      (Node (Leaf 3) (Leaf 4)))
                (Leaf 5))
          (Node (Node (Leaf 6) (Leaf 7))
                (Node (Leaf 8)
                      (Node (Leaf 9) (Leaf 10))))

sizeTests :: [Test]
sizeTests = map TestCase
  [ assertEqual "t0" 1 (size t0)
  , assertEqual "t1" 3 (size t1)
  , assertEqual "t2" 4 (size t2)
  , assertEqual "t3" 4 (size t3)
  , assertEqual "t4" 10 (size t4)
  ]

balancedTests :: [Test]
balancedTests = map TestCase
  [ assertEqual "t0" True (balanced t0)
  , assertEqual "t1" True (balanced t1)
  , assertEqual "t2" True (balanced t2)
  , assertEqual "t3" False (balanced t3)
  , assertEqual "t4" False (balanced t4)
  ]

runTests :: [Test] -> IO Counts
runTests ts = runTestTT $ TestList ts

main = runTests $ sizeTests ++ balancedTests

