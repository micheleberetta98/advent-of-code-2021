module Main where

import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Matrix

main :: IO ()
main = do
  m <- fromLists . map (map digitToInt) . lines <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (riskLevel m)
  putStr "Answer 2:  " >> print (largestBasins m)

------------ Solutions

riskLevel :: Matrix Int -> Int
riskLevel = sum . map (+1) . lowPoints
  where lowPoints m = map (m !) (lowIndices m)

largestBasins :: Matrix Int -> Int
largestBasins m = product . take 3 . sortBy (flip compare) . map getBasinSize . lowIndices $ m
  where getBasinSize ij = evalState (basinSize m ij) (False <$ m)

basinSize :: Matrix Int -> (Int, Int) -> State (Matrix Bool) Int
basinSize m coords = do
  visited <- get
  if visited ! coords
    then pure 0
    else do
      let notVisited = filter (not . (visited !)) . filter isViable $ neighbouringIndices m coords
      modify' (setElem True coords)
      rest <- sum <$> traverse (basinSize m) notVisited
      pure (1 + rest)
  where
    isViable ij = let k = m ! ij in k > m ! coords && k < 9

------------ Utils

lowIndices :: Matrix Int -> [(Int, Int)]
lowIndices = filter <$> isLow <*> indices
  where indices m = [(i, j) | i <- [1..nrows m], j <- [1..ncols m]]

isLow :: Matrix Int -> (Int, Int) -> Bool
isLow m pos = all (> m ! pos) neighbours
  where neighbours = map (m !) (neighbouringIndices m pos)

neighbouringIndices :: Matrix a -> (Int, Int) -> [(Int, Int)]
neighbouringIndices m (i, j) = filter valid [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]
  where valid (x, y) = x >= 1 && x <= nrows m && y >= 1 && y <= ncols m
