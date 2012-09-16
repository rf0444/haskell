module HogeExpr where

import Control.Applicative (pure, (<$>), (<*>), (<*), (*>), (<|>))
import Text.Parsec (many1, chainl1, char, digit, spaces, eof, parse)
import Text.ParserCombinators.Parsec (Parser)

data Expr =
   Number Integer
 | Add Expr Expr
 | Sub Expr Expr
 | Mult Expr Expr
 | Div Expr Expr
 deriving (Show, Eq)

number :: Parser Expr
number = Number . read <$> many1 digit

expr :: Parser Expr
expr = chainl1 term $ char '+' *> pure Add <|> char '-' *> pure Sub

term :: Parser Expr
term = chainl1 factor $ char '*' *> pure Mult <|> char '/' *> pure Div

factor :: Parser Expr
factor = char '(' *> expr <* char ')' <|> number

eval :: Expr -> Integer
eval (Number x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mult x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

