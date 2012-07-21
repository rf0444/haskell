{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-} -- for test

-- 前提
class Print a where
  printWeak :: a -> String
  printStrong :: a -> String

class Banner a where
  showWithParen :: a -> String
  showWithAster :: a -> String

-- Adaper
instance (Banner a) => Print a where
  printWeak = showWithParen
  printStrong = showWithAster

-- for test
instance Banner String where
  showWithParen s = "(" ++ s ++ ")"
  showWithAster s = "*" ++ s ++ "*"

main :: IO ()
main = do
  putStrLn $ printWeak bn
  putStrLn $ printStrong bn
 where
  bn :: String
  bn = "hoge"

