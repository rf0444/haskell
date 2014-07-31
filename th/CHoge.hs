{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module CHoge
  ( CHoge(hoge)
  , deriveCHoge
  ) where

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class CHoge a where
  hoge :: a -> String

deriveCHoge :: Name -> DecsQ
deriveCHoge name = sequence
  [ instanceD (cxt []) (appT (conT ''CHoge) (conT name))
    [ funD 'hoge
      [ clause [wildP] (normalB $ stringE "hoge") []
      ]
    ]
  ]
