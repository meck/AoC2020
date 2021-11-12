module Day1 (day01a, day01b) where

import AoC.Parsing
import Control.Monad (replicateM)

nCombi :: (Num p, Eq p) => Int -> [p] -> p
nCombi n xs = product . head $ [ys | ys <- replicateM n xs, sum ys == 2020]

day01a :: String -> String
day01a = show . nCombi 2 . run (sepNL int)

day01b :: String -> String
day01b = show . nCombi 3 . run (sepNL int)
