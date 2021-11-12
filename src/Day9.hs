module Day9 (day09a, day09b) where

import AoC.Parsing (int, run, sepNL)
import Data.List (find, inits, sortOn, tails)
import Data.Maybe (catMaybes, fromJust)
import Safe (atMay, headMay, tailMay)

validXMAS :: (Eq a, Num a) => [a] -> a -> Bool
validXMAS preamble y = any (`elem` preHalf) preamble
  where
    preHalf = filter ((/= y) . (* 2)) $ (y -) <$> preamble

checkXMAS :: (Eq p, Num p) => Int -> [p] -> Maybe p
checkXMAS preLen xs = do
  needle <- xs `atMay` max 0 preLen
  let preamble = take preLen xs
  if validXMAS preamble needle
    then checkXMAS preLen =<< tailMay xs
    else pure needle

crackXMAS :: (Ord a, Num a) => Int -> [a] -> Maybe a
crackXMAS preLen xs = do
  firstErr <- checkXMAS preLen xs
  let sumInits = find ((== firstErr) . sum) . inits
  shortest <- headMay $ sortOn length $ filter ((> 1) . length) $ catMaybes $ sumInits <$> tails xs
  pure $ minimum shortest + maximum shortest

day09a :: String -> String
day09a = show . fromJust . checkXMAS 25 . run (sepNL int)

day09b :: String -> String
day09b = show . fromJust . crackXMAS 25 . run (sepNL int)
