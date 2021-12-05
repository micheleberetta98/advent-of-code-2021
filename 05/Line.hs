{-# LANGUAGE TupleSections #-}

module Line where

import           Data.Char          (isDigit)
import           Text.Parsec        hiding (Line)
import           Text.Parsec.String (Parser)

------------ Types

type Point = (Int, Int)
type Line = [Point]

------------ Functions

line :: Point -> Point -> Line
line (x1, y1) (x2, y2)
  | x1 == x2  = map (x1,) $ linearIn y1 y2
  | y1 == y2  = map (,y1) $ linearIn x1 x2
  | otherwise = zip (linearIn x1 x2) (linearIn y1 y2)
  where
    linearIn a b = if a < b then [a..b] else [a,a-1..b]

------------ Parsers

parseLines :: String -> Either ParseError [Line]
parseLines = parse (pLine `sepBy` newline) ""
  where
    pLine = line <$> pPoint <*> (string " -> " *> pPoint)
    pPoint = (,) <$> number <*> (char ',' *> number)
    number = read <$> many1 (satisfy isDigit)
