module Day11 (day11a, day11b) where

import AoC.Util (firstEq, mkCordsGrid, neighbourCords, (+^))
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, isJust, isNothing)

type SeatArea = M.Map (Int, Int) SeatState

type NeighbourFn = SeatArea -> (Int, Int) -> [SeatState]

type Rules = Int -> SeatState -> SeatState

data SeatState = Floor | Empty | Occupied | Wall deriving (Eq, Show)

nearNeighbours :: NeighbourFn
nearNeighbours sa n = catMaybes $ (sa M.!?) <$> ((n +^) <$> neighbourCords)

rulesA :: Rules
rulesA 0 Empty = Occupied
rulesA n Occupied | n >= 4 = Empty
rulesA _ s = s

longNeighbours :: NeighbourFn
longNeighbours sa n = catMaybes $ head $ dropWhile (any isNothing) $ scanl go (repeat Nothing) steps
  where
    nearest = (n +^) <$> neighbourCords
    steps = iterate (fmap (uncurry (+^)) . zip neighbourCords) nearest
    go acc cords = zipWith go' cords acc
      where
        go' c mRes
          | isJust mRes = mRes -- Already in line of sight
          | isNothing mNext = Just Wall -- Outside the grid
          | mNext == Just Floor = Nothing -- Floor, keep iterating
          | otherwise = mNext -- Must be result
          where
            mNext = sa M.!? c

rulesB :: Rules
rulesB 0 Empty = Occupied
rulesB n Occupied | n >= 5 = Empty
rulesB _ s = s

tickSeats :: Rules -> NeighbourFn -> SeatArea -> SeatArea
tickSeats rules f sa = M.mapWithKey (rules . nOccupedNeigh) sa
  where
    nOccupedNeigh :: (Int, Int) -> Int
    nOccupedNeigh = length . filter (== Occupied) . f sa

getGrid :: [String] -> SeatArea
getGrid = M.mapMaybe id . mkCordsGrid toSeat
  where
    toSeat c = case c of
      'L' -> Just Empty
      '.' -> Just Floor
      '#' -> Just Occupied
      _ -> Nothing

solve :: Rules -> NeighbourFn -> SeatArea -> Int
solve rules neighbourFn = length . M.filter (== Occupied) . firstEq . iterate (tickSeats rules neighbourFn)

day11a :: String -> String
day11a = show . solve rulesA nearNeighbours . getGrid . lines

day11b :: String -> String
day11b = show . solve rulesB longNeighbours . getGrid . lines
