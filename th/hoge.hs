{-# LANGUAGE TemplateHaskell #-}

import CHoge

data Hoge = Hoge

deriveCHoge ''Hoge

main :: IO ()
main = putStrLn $ hoge Hoge
