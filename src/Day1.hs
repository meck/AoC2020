module Day1 (day01a, day01b) where

import Control.Monad (replicateM)
import qualified Parsing as P

nCombi :: (Num p, Eq p) => Int -> [p] -> p
nCombi n xs = product . head $ [ys | ys <- replicateM n xs, sum ys == 2020]

day01a :: String -> String
day01a i = show $ nCombi 2 $ P.run P.integer <$> lines i

day01b :: String -> String
day01b i = show $ nCombi 3 $ P.run P.integer <$> lines i
