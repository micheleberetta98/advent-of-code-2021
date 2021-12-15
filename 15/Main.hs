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
  putStr "Answer 2:  " >> print (solve2 m)


solve1 :: Matrix Int -> Maybe Int
solve1 m = sum . map (m !) <$> shortestPath m

solve2 :: Matrix Int -> Maybe Int
solve2 = solve1 . expand

shortestPath :: Matrix Int -> Maybe [Pos]
shortestPath m = aStar (neighbouringIndices' m) dist estimateFromEnd (== (endi, endj)) (0, 0)
  where
    dist _ b = m ! b
    (endi, endj) = (rows m - 1, cols m - 1)
    estimateFromEnd (i, j) = ceiling . sqrt $ fromIntegral ((endi - i) ^ 2 + (endj - j) ^ 2)
    neighbouringIndices' m = fromList . neighbouringIndices m

expand :: Matrix Int -> Matrix Int
expand = fromLists . expandVer . expandHor . toLists

expandHor :: [[Int]] -> [[Int]]
expandHor = map $ \xs -> xs ++ map (inc 1) xs ++ map (inc 2) xs ++ map (inc 3) xs ++ map (inc 4) xs

expandVer :: [[Int]] -> [[Int]]
expandVer xs = xs ++ map (map (inc 1)) xs ++ map (map (inc 2)) xs ++ map (map (inc 3)) xs ++ map (map (inc 4)) xs

inc :: Int -> Int -> Int
inc k x = let x' = x + k in if x' > 9 then x' - 9 else x'

parse :: String -> Matrix Int
parse = fromLists . map (map digitToInt) . lines
