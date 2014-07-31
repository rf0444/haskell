{-# LANGUAGE TemplateHaskell #-}

import CHoge

data Hoge = Hoge
  { name :: String
  , note :: String
  }
deriveCHoge ''Hoge

data Piyo = Piyo
  { val1 :: Int
  , val2 :: Int
  }
deriveCHoge ''Piyo

main :: IO ()
main = do
  hoge $ Hoge "aa" "bbb"
  hoge $ Piyo 123 456
