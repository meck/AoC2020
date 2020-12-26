module Day2 (day02a, day02b) where

import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char

data Pass = Pass Int Int Char String

isValid :: Pass -> Bool
isValid (Pass minN maxN c s) = (n >= minN) && (n <= maxN)
  where
    n = length $ filter (== c) s

isValid2 :: Pass -> Bool
isValid2 (Pass a b c s) = (== 1) $ length $ filter (== c) [s !! pred a, s !! pred b]

pass :: Parser Pass
pass = do
  mi <- int
  ma <- (char '-') >> int
  c <- spaceChar >> lowerChar
  s <- chunk ": " >> some lowerChar
  pure $ Pass mi ma c s

day02a :: String -> String

day02a = show . length . filter isValid . run (sepNL pass)

day02b :: String -> String
day02b = show . length . filter isValid2 . run (sepNL pass)
