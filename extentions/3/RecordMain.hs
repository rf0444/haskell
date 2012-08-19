{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified RecordA
import qualified RecordB
import qualified RecordC

createA :: RecordA.A
createA = RecordA.A {..}
 where
  a = 1
  b = "b"
  c = 100

-- A をそのまま B に変換
toB1 :: RecordA.A -> RecordB.B
toB1 (RecordA.A {..}) = RecordB.B {..}

-- A の一部を上書きして B に変換
toB2 :: RecordA.A -> RecordB.B
toB2 (RecordA.A {..}) = RecordB.B {..}
 where
  b = "bb"

-- A を C に変換 (C.c = "aaa") (A.c は使わない)
toC1 :: RecordA.A -> RecordC.C
toC1 (RecordA.A {..}) = RecordC.C {..}
 where
  c = "aaa"

-- A を C に変換 (C.c = show A.c)
toC2 :: RecordA.A -> RecordC.C
toC2 (RecordA.A { c = ca, ..}) = RecordC.C {..}
 where
  c = show ca

main :: IO ()
main = do
  print $ createA
  print $ toB1 $ createA
  print $ toB2 $ createA
  print $ toC1 $ createA
  print $ toC2 $ createA

