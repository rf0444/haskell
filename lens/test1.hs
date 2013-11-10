{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Control.Lens

data Hoge = Hoge
  { _hoge :: Int
  , _piyo :: String
  }
  deriving Show
$(makeLenses ''Hoge)

main = do
  print $ f g4 (1,2)
  print $ f h3 $ Hoge 3 "Hoge"
 where
  f :: Getting a s a -> s -> a
  f = view
  g1 :: IndexedLens' Int (a,b) a
  g1 = _1
  -- IndexedLens < Lens
  g2 :: Lens' (a,b) a
  g2 = g1
  -- Lens < Getter
  g3 :: Getter (a, b) a
  g3 = g2
  -- Getter < Getting
  g4 :: Getting a (a, b) a
  g4 = g3
  -- Lens = Functor f => (a -> f b) -> s -> f t
  h1 :: Functor f => (Int -> f Int) -> Hoge -> f Hoge
  h1 = hoge
  h2 :: Lens' Hoge Int
  h2 = h1
  h3 :: Functor f => (Int -> f Int) -> Hoge -> f Hoge
  h3 = h2
