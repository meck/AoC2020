module Day6 (day06a, day06b) where

import Data.List (intersect, nub)
import Data.List.Split (splitOn)

day06a :: String -> String
day06a = show . sum . fmap (length . nub . concat . lines) . splitOn "\n\n"

day06b :: String -> String
day06b = show . sum . fmap (length . common . lines) . splitOn "\n\n"
  where
    common = foldr intersect ['a' .. 'z']
