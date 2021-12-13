module Main where

import           Data.Char          (isDigit)
import           Data.Set           (fromList)
import           Dot
import           Text.Parsec        (char, choice, many1, newline, parse,
                                     satisfy, sepBy, sepEndBy, string)
import           Text.Parsec.String (Parser)

main :: IO ()
main = do
  Right (dots, folds) <- parse content "" <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (afterFirstFold folds dots)

------------ Solutions

afterFirstFold :: [Fold] -> Dots -> Int
afterFirstFold folds = length . foldAlong (head folds)

------------ Parser

content :: Parser (Dots, [Fold])
content = (,) <$> dots <*> (newline *> fold `sepBy` newline)
  where
    dots = fromList <$> dot `sepEndBy` newline

dot :: Parser Dot
dot = Dot <$> ((,) <$> number <*> (char ',' *> number))

fold :: Parser Fold
fold = string "fold along " *> choice
  [ XFold <$> (string "x=" *> number)
  , YFold <$> (string "y=" *> number)
  ]

number :: Parser Int
number = read <$> many1 (satisfy isDigit)
