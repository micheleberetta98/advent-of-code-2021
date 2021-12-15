{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Char
import           Data.Graph.AStar
import           Data.HashSet     (fromList)
import           Matrix

main :: IO ()
main = do
  m <- parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (solve1 m)


solve1 :: Matrix Int -> Maybe Int
solve1 m = sum . map (m !) <$> shortestPath m

shortestPath :: Matrix Int -> Maybe [Pos]
shortestPath m = aStar (neighbouringIndices' m) dist estimateFromEnd (== (endi, endj)) (0, 0)
  where
    dist _ b = m ! b
    (endi, endj) = (rows m - 1, cols m - 1)
    estimateFromEnd (i, j) = ceiling . sqrt $ fromIntegral ((endi - i) ^ 2 + (endj - j) ^ 2)
    neighbouringIndices' m = fromList . neighbouringIndices m

parse :: String -> Matrix Int
parse = fromLists . map (map digitToInt) . lines
