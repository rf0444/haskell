{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances #-}

module CHoge
  ( CHoge(hoge)
  , deriveCHoge
  ) where

import Language.Haskell.TH

class CHoge a where
  hoge :: a -> IO ()

deriveCHoge :: Name -> DecsQ
deriveCHoge name = do
  (TyConI (DataD _ _ _ [RecC cname fields] _)) <- reify name
  [d|
    instance CHoge $(conT name) where
      hoge x = $(doE $
        [ noBindS [| putStrLn $(showNameE cname) |]
        ] ++ map (noBindS . printFieldE 'x) fields)
    |]
  where
    printFieldE vname (fname, _, _) =
      [| putStrLn $ $(showNameE fname) ++ ": " ++ $(showValueE vname fname) |]
    showNameE fname = stringE $ nameBase fname
    showValueE vname fname = [| show $ $(varE fname) $(varE vname) |]
