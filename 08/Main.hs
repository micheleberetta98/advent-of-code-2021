module Main where

import           Data.Bifunctor
import           Decoding

main :: IO ()
main = do
  tuples <- parse <$> readFile "input.txt"
  putStr "Answer 1:    " >> print (countUniqueDigits tuples)
  putStr "Answer 2:    " >> print (decode tuples)

------------ Part 1

countUniqueDigits :: [([Wires], [Wires])] -> Int
countUniqueDigits = sum . map (length . filter isUniqueNumber . snd)
  where isUniqueNumber xs = length xs `elem` [2, 4, 3, 7]

------------ Part 2

decode :: [([Wires], [Wires])] -> Int
decode = sum . map decode'
  where decode' (signals, output) = decodeWith (findSolution signals) output

------------ Parsing

parse :: String -> [([Wires], [Wires])]
parse = map parseTuple . lines
  where parseTuple = bimap words (tail . words) . break (== '|')
