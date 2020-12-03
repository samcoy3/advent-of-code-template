module Util.Parsers where

import Data.Attoparsec.Text
import Data.Map (Map)
import qualified Data.Map as Map

{-
This module contains a list of parsers and combinators which are likely to be useful for Advent of Code problems.
-}

------------ PARSERS ------------

-- Takes a "mapper" "function that might map a char to a datatype, and an initial index (usually 0 or 1)
-- Returns a parser that returns a map from coordinates to all instances where the function returns Just
coordinateParser :: (Char -> Maybe a) -> Int -> Parser (Map (Int, Int) a)
coordinateParser mapper start = coordinateParser' start start
  where
    coordinateParser' x y =
      choice
        -- First we look for a line break, and we reset the coordinates appropriately
        [ endOfLine >> coordinateParser' start (y + 1),
          -- Then we look for a character, and map it
          anyChar >>= (\c -> addToMap mapper x y c <$> coordinateParser' (x + 1) y),
          -- Catches the EOF
          return Map.empty
        ]
    addToMap mapper x y c = Map.alter (const (mapper c)) (x, y)

------------ COMBINATORS ------------

-- Takes a parser and a separator. Parses one instance of the parser before the separator and one afterwards, returning the parsed values as a pair.
around :: Parser a -> Parser b -> Parser (a, a)
around p sep = do
  a <- p
  sep
  b <- p
  return (a, b)
