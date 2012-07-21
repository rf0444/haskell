{-# LANGUAGE RecordWildCards #-}

data Ele = Ele { a :: Int, b :: Int, s1 :: String, s2 :: String }

create :: Ele
create = Ele { b = 2, ..}
 where
  a = 3
  s1 = "hoge"
  s2 = "piyo"

str :: Ele -> String
str (Ele { a = 1, ..}) = s1
str (Ele { a = 2, ..}) = s2
str (Ele { a = 3, ..}) = s1 ++ s2
str (Ele {..}) = s1

main = putStrLn . str $ create

