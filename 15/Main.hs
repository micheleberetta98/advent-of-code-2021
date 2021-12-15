module Main where

import           Data.Char
import           Data.Graph.AStar
import           Data.List
import           SquareMatrix

main :: IO ()
main = do
  m <- fromLists . parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (solveExpandedBy 1 m)
  putStr "Answer 2:  " >> print (solveExpandedBy 5 m)

solveExpandedBy :: Int -> SquareMatrix -> Maybe Int
solveExpandedBy k m = foldl' (+) 0 . map (m <!>) <$> shortestPathExpandedBy k m

shortestPathExpandedBy :: Int -> SquareMatrix -> Maybe [Pos]
shortestPathExpandedBy k m@(SquareMatrix d _) = aStar (neighbouringIndices k m) dist estimateFromEnd (== end) (0, 0)
  where
    dist _ b = m <!> b
    end@(endi, endj) = (k*d - 1, k*d - 1)
    estimateFromEnd (i, j) = 2 * abs (endi - i) + abs (endj - j)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines
