{-# LANGUAGE OverloadedStrings #-}

module Day24 (day24a, day24b) where

import AoC.Parsing
import AoC.Util (occurencesLookup)
import Control.Applicative (Alternative (some), (<|>))
import qualified Data.Set as S
import Text.Megaparsec.Char (string)

data CubeCord = CC {q :: Int, r :: Int, s :: Int} deriving (Eq, Show, Ord)

instance Semigroup CubeCord where
  (CC q1 r1 s1) <> (CC q2 r2 s2) = CC (q1 + q2) (r1 + r2) (s1 + s2)

instance Monoid CubeCord where
  mempty = CC 0 0 0

e, se, sw, w, nw, ne :: CubeCord
e = CC 1 0 (-1)
se = CC 0 1 (-1)
sw = CC (-1) 1 0
w = CC (-1) 0 1
nw = CC 0 (-1) 1
ne = CC 1 (-1) 0

step :: Parser CubeCord
step =
  (string "e" >> pure e)
    <|> (string "se" >> pure se)
    <|> (string "sw" >> pure sw)
    <|> (string "w" >> pure w)
    <|> (string "nw" >> pure nw)
    <|> (string "ne" >> pure ne)

initalFloor :: [[CubeCord]] -> [(CubeCord, Integer)]
initalFloor = filter (odd . snd) . occurencesLookup . fmap mconcat

type Floor = S.Set CubeCord

neighCords :: CubeCord -> Floor
neighCords c = S.map (c <>) $ S.fromList [e, se, sw, w, nw, ne]

stepFloor :: Floor -> Floor
stepFloor current = S.filter (shouldBeBlack current) currentWithNeighbours
  where
    currentWithNeighbours = foldr (\c acc -> acc `S.union` neighCords c) current current

shouldBeBlack :: Floor -> CubeCord -> Bool
shouldBeBlack currentFloor tile
  | isBlack = nBlackNeigh >= 1 && nBlackNeigh <= 2
  | not isBlack = nBlackNeigh == 2
  | otherwise = False
  where
    isBlack = tile `S.member` currentFloor
    nBlackNeigh = S.size $ neighCords tile `S.intersection` currentFloor

solveB :: [(CubeCord, b)] -> Int
solveB = S.size . (!! 100) . iterate stepFloor . S.fromList . fmap fst

day24a :: String -> String
day24a = show . length . initalFloor . run (sepNL (some step))

day24b :: String -> String
day24b = show . solveB . initalFloor . run (sepNL (some step))
