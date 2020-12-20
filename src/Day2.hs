module Day2 (day02a, day02b) where

import qualified Parsing as P
import Text.ParserCombinators.ReadP

data Pass = Pass Int Int Char String

isValid :: Pass -> Bool
isValid (Pass minN maxN c s) = (n >= minN) && (n <= maxN)
  where
    n = length $ filter (== c) s

isValid2 :: Pass -> Bool
isValid2 (Pass a b c s) = (== 1) $ length $ filter (== c) [s !! pred a, s !! pred b]

pass :: ReadP Pass
pass = do
  mi <- P.integer
  ma <- char '-' >> P.integer
  c <- char ' ' >> get
  s <- string ": " >> many1 get
  pure $ Pass mi ma c s

day02a :: String -> String
day02a = show . length . filter isValid . fmap (P.run pass) . lines

day02b :: String -> String
day02b = show . length . filter isValid2 . fmap (P.run pass) . lines
