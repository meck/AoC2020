module Parsing (Parser, run, lexeme, sepNL, int, hex, signedInt) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Simplest type of parser
type Parser = Parsec Void String

-- Run Parser until eof
run :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s p -> s -> p
run p s = case runParser (p <* eof) "" s of
  (Left err) -> error $ errorBundlePretty err
  (Right res) -> res

-- Space Between Tokens
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1

-- newline Between Tokens
sepNL :: Parser a -> Parser [a]
sepNL = flip sepBy1 newline

-- Integer
int :: Parser Int
int = L.decimal

-- Integer
hex :: Parser Int
hex = L.hexadecimal

-- Signed Integer
-- No space between sign
signedInt :: Parser Int
signedInt = L.signed (pure ()) int
