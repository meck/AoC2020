module Day3 (day03a, day03b) where

pathForStep :: (Int, Int) -> [[Char]] -> Int
pathForStep (hs, vs) slope = length $ filter isTree path
  where
    isTree (x, y) = ((slope !! y) !! x) == '#'
    path = tail $ zip (flip mod w <$> [0, hs ..]) [0, vs .. pred h]
    w = length $ head slope
    h = length slope

solveB :: [[Char]] -> Int
solveB slope = product $ flip pathForStep slope <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

day03a :: String -> String
day03a = show . pathForStep (3, 1) . lines

day03b :: String -> String
day03b = show . solveB . lines
