module Main where

import           Data.Char
import           Data.Maybe
import           Matrix

main :: IO ()
main = do
  m <- parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (riskLevel m)

riskLevel :: Matrix Int -> Int
riskLevel = sum . map (+1) . catMaybes . lowPoints

lowPoints :: Matrix Int -> [Maybe Int]
lowPoints m = map (m `at`) $ filter (isLow m) indices
  where
    (rows, cols) = dimensions m
    indices = [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]

isLow :: Matrix Int -> (Int, Int) -> Bool
isLow m pos = all (> x) (neighbours m pos)
  where Just x = m `at` pos

parse :: String -> Matrix Int
parse = fromList . map (map digitToInt) . lines
