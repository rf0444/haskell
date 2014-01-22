import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>))
import qualified Data.List as L
import qualified Data.Map.Strict as M

data Record = Record
  { ident :: Int
  , parentId :: Int
  , content :: String
  } deriving Show

data Node a = Node
  { value :: a
  , children :: Tree a
  } deriving Show

type Tree a = [Node a]

size :: Tree a -> Int
size = sum . map (succ . size . children)

mkTree :: [Record] -> Tree Record
mkTree xs = children' 0
  where
    children' i = maybe [] (map (Node <$> id <*> children' . ident)) (M.lookup i pmap)
    pmap = mkBag $ map (parentId &&& id) xs

mkBag :: Ord k => [(k, v)] -> M.Map k [v]
mkBag = foldr insertOrAdd M.empty
  where
    insertOrAdd (k, v) m
      | M.member k m = M.adjust (v:) k m
      | otherwise = M.insert k [v] m

filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree p = concatMap filterTree'
  where
    filterTree' (Node v c) = if not (null newc) || p v then [Node v newc] else []
      where
        newc = filterTree p c

main :: IO ()
main = do
  print tree
  print $ size tree
  print $ tree'
  print $ size $ tree'
  where
    byContentOrRoot text = (||) <$> (text `L.isInfixOf`) . content <*> (== 0) . parentId
    tree = mkTree list
    tree' = filterTree (byContentOrRoot "test") tree
    list =
      [ Record 1 0 "/1 root1"
      , Record 2 1 "/1/2 - test"
      , Record 3 2 "/1/2/3 - hoge"
      , Record 4 1 "/1/4 - piyo"
      , Record 5 4 "/1/4/5 - test"
      , Record 6 5 "/1/4/5/6 - aaa"
      , Record 7 5 "/1/4/5/7 - bbb"
      , Record 10 0 "/10 - root2"
      , Record 11 10 "/10/11 - xxx"
      , Record 12 11 "/10/11/12 - yyy"
      , Record 13 11 "/10/11/13 - zzz"
      ]
