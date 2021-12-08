module Main where

import           Data.Bifunctor

main :: IO ()
main = do
  tuples <- parse <$> readFile "input.txt"
  putStr "Answer 1:    " >> print (countUniqueDigits tuples)

------------ Part 1

countUniqueDigits :: [([String], [String])] -> Int
countUniqueDigits = sum . map (length . filter isUniqueNumber . snd)
  where
    isUniqueNumber xs = length xs `elem` [2, 4, 3, 7]

------------ Parsing

parse :: String -> [([String], [String])]
parse = map parseTuple . lines
  where parseTuple = bimap words words . break (== '|')
