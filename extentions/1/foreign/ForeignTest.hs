
module ForeignTest where

foreign export ccall foo :: Int -> Int

foo :: Int -> Int
foo = length . show

