module Day4 (day04a, day04b) where

import AoC.Parsing
import Control.Applicative.Permutations
import Data.Char
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Pass1 = Pass1 String String String String String String String (Maybe String) deriving (Eq, Show)

stringWithLabel :: String -> Parser String
stringWithLabel l = string (l <> ":") >> takeWhile1P Nothing (not . isSpace)

pass1 :: Parser Pass1
pass1 =
  intercalateEffect spaceChar $
    Pass1 <$> toPermutation (stringWithLabel "byr")
      <*> toPermutation (stringWithLabel "iyr")
      <*> toPermutation (stringWithLabel "eyr")
      <*> toPermutation (stringWithLabel "hgt")
      <*> toPermutation (stringWithLabel "hcl")
      <*> toPermutation (stringWithLabel "ecl")
      <*> toPermutation (stringWithLabel "pid")
      <*> toPermutationWithDefault Nothing (Just <$> stringWithLabel "cid")

digitRange :: String -> Int -> Int -> Parser Int
digitRange lab l h = do
  i <- string (lab <> ":") >> L.decimal 
  if i >= l && i <= h
    then pure i
    else fail "Bad range"

newtype Byr = Byr Int deriving (Eq, Show)

byr :: Parser Byr
byr = Byr <$> digitRange "byr" 1920 2002

newtype Iyr = Iyr Int deriving (Eq, Show)

iyr :: Parser Iyr
iyr = Iyr <$> digitRange "iyr" 2010 2020

newtype Eyr = Eyr Int deriving (Eq, Show)

eyr :: Parser Eyr
eyr = Eyr <$> digitRange "eyr" 2020 2030

newtype Hgt = Hgt Int deriving (Eq, Show)

hgt :: Parser Hgt
hgt = do
  h <- string "hgt:" >> L.decimal 
  u <- string "cm" <|> string "in"
  let res
        | u == "cm" && h >= 150 && h <= 194 = pure $ Hgt h
        | u == "in" && h >= 59 && h <= 76 = pure $ Hgt h
        | otherwise = fail "Bad Eyr"
  res

newtype Hcl = Hcl String deriving (Eq, Show)

hcl :: Parser Hcl
hcl = Hcl <$> (string "hcl:#" >> count 6 hexDigitChar)

newtype Ecl = Ecl String deriving (Eq, Show)

ecl :: Parser Ecl
ecl =
  fmap Ecl $
    string "ecl:"
      >> string "amb"
        <|> string "blu"
        <|> string "brn"
        <|> string "gry"
        <|> string "grn"
        <|> string "hzl"
        <|> string "oth"

newtype Pid = Pid String deriving (Eq, Show)

pid :: Parser Pid
pid = Pid <$> (string "pid:" >> count 9 digitChar)

newtype Cid = Cid String deriving (Eq, Show)

cid :: Parser Cid
cid = fmap Cid $ string "cid:" >> takeWhile1P Nothing (not . isSpace)

data Pass2 = Pass2 Byr Iyr Eyr Hgt Hcl Ecl Pid (Maybe Cid) deriving (Eq, Show)

pass2 :: Parser Pass2
pass2 =
  intercalateEffect spaceChar $
    Pass2 <$> toPermutation byr
      <*> toPermutation iyr
      <*> toPermutation eyr
      <*> toPermutation hgt
      <*> toPermutation hcl
      <*> toPermutation ecl
      <*> toPermutation pid
      <*> toPermutationWithDefault Nothing (Just <$> cid)

day04a :: String -> String
day04a = show . length . mapMaybe (parseMaybe pass1) . splitOn "\n\n"

day04b :: String -> String
day04b = show . length . mapMaybe (parseMaybe pass2) . splitOn "\n\n"
-- day04b = show . length . fmap (run  pass2) . splitOn "\n\n"
