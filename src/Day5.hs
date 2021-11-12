{-# LANGUAGE LambdaCase #-}

module Day5 (day05a, day05b) where

import AoC.Parsing
import Data.Bits
import Data.List (foldl', (\\))
import Text.Megaparsec
import Text.Megaparsec.Char

bitsToWord :: Foldable t => t Bool -> Int
bitsToWord = foldl' go zeroBits
  where
    go acc b
      | b = next `setBit` 0
      | otherwise = next
      where
        next = shiftL acc 1

seatNum :: Parser [Bool]
seatNum = do
  a <- count 7 ud
  b <- count 3 lr
  pure $ a <> b
  where
    ud =
      anySingle >>= \case
        'F' -> pure False
        'B' -> pure True
        _ -> fail "Bad value"
    lr =
      anySingle >>= \case
        'R' -> pure True
        'L' -> pure False
        _ -> fail "Bad value"

missing :: (Ord a, Enum a) => [a] -> [a]
missing xs = [minimum xs .. maximum xs] \\ xs

day05a :: String -> String
day05a = show . maximum . fmap bitsToWord . run (sepEndBy1 seatNum newline)

day05b :: String -> String
day05b = show . missing . fmap bitsToWord . run (sepEndBy1 seatNum newline)
