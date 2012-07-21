
-- 前提
class Print a where
  printWeak :: a -> String
  printStrong :: a -> String

data Banner = Bn String

showWithParen :: Banner -> String
showWithParen (Bn s) = "(" ++ s ++ ")"

showWithAster :: Banner -> String
showWithAster (Bn s) = "*" ++ s ++ "*"

-- Adaper
instance Print Banner where
  printWeak = showWithParen
  printStrong = showWithAster

-- for test
main :: IO ()
main = do
  putStrLn $ printWeak bn
  putStrLn $ printStrong bn
 where
  bn :: Banner
  bn = Bn "hoge"

