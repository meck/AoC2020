{-# LANGUAGE OverloadedStrings #-}

module Day18 (day18a, day18b) where

import AoC.Parsing (Parser, int, parens, run, sepNL, symbol)
import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr

add :: Operator Parser Int
add = InfixL ((+) <$ symbol "+")

mul :: Operator Parser Int
mul = InfixL ((*) <$ symbol "*")

mkParser :: [[Operator Parser Int]] -> Parser Int
mkParser table = expr
  where
    term = parens expr <|> int
    expr = makeExprParser term table

day18a :: String -> String
day18a = show . sum . run (sepNL $ mkParser [[add, mul]])

day18b :: String -> String
day18b = show . sum . run (sepNL $ mkParser [[add], [mul]])
