{-# LANGUAGE ExistentialQuantification #-}

data Ele = forall a . (Show a) => Ele a
instance Show Ele where
  show (Ele x) = show x

eles :: [Ele]
eles = [Ele "hoge", Ele 1, Ele (3,6), Ele $ Just 2, Ele [4,3], Ele ["a", "b"], Ele 1.23]

main = putStrLn . unlines $ show `map` eles

