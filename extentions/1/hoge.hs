{-# LANGUAGE GADTs, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE BangPatterns, ViewPatterns #-}

import Control.Monad (mfilter)

-- require extention of GADTs
data List a where
  INil :: List Int
  CNil :: List Char
  Cons :: a -> List a -> List a

-- error occured because the type of y is different from the type of x
--myId :: a -> a
--myId x = y
-- where
--  y :: a
--  y = x

-- require extention of ScopedTypeVariables
myId :: forall a. a -> a
myId x = y
 where
  y :: a
  y = x

-- require extention of EmptyDataDecls (default on)
data EmpData
data EmpDataF a

-- require extention of TypeFamilies
type family TypeConv a :: *
type instance TypeConv EmpData = Int 
type instance TypeConv (EmpDataF a) = a 

-- require extention of TypeFamilies
data family MyDF a
data instance MyDF Int = MyDF1 Int | MyDF2 Int Int
newtype instance MyDF EmpData = MyDF3 Bool
newtype instance MyDF (EmpDataF a) = MyDF4 a


class CHoge a where
  hoge :: a -> a

-- require extention of FlexibleInstances
instance CHoge (Maybe Int) where
  hoge _ = Nothing

type SHoge = String
-- require extention of TypeSynonymInstances
instance CHoge SHoge where
  hoge = id

-- require extention of MultiParamTypeClasses
class CCHoge a b where
  choge :: a -> b

-- require extention of FlexibleContexts
ihoge :: (CCHoge a Int) => a -> Int
ihoge _ = 0


-- require extention of BangPatterns
myseq :: a -> b -> b
myseq !x y = y

-- require extention of PatternGuards (default on)
justOddOrElse :: Maybe Int -> Int -> Int
justOddOrElse x y
 | Just a <- x, odd a = a
 | otherwise = y

-- require extention of ViewPatterns
justOddOrElse' :: Maybe Int -> Int -> Int
justOddOrElse' (mfilter odd -> Just a) _ = a
justOddOrElse' _ b = b


main = return ()

