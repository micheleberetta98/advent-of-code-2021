module Main where

import           Data.Char
import           Data.List
import           Matrix

main :: IO ()
main = do
  m <- parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (riskLevel m)
  putStr "Answer 2:  " >> print (largestBasins m)

------------ Solutions

riskLevel :: Matrix Int -> Int
riskLevel = sum . map (+1) . lowPoints
  where lowPoints m = map (m `at`) (lowIndices m)

largestBasins :: Matrix Int -> Int
largestBasins = product . take 3 . reverse . sort . map length . basins
  where basins m = map (basin (False <$ m) m) (lowIndices m)

basin :: Matrix Bool -> Matrix Int -> (Int, Int) -> [(Int, Int)]
basin visited m currentCoords = nub $ currentCoords : concatMap (basin visited' m) notVisited
  where
    visited' = set currentCoords True visited
    notVisited = filter isViable $ neighbouringIndices m currentCoords
    isViable ij =
      let k = m `at` ij
          x = m `at` currentCoords
      in not (visited `at` ij) && k > x && k < 9

------------ Utils

lowIndices :: Matrix Int -> [(Int, Int)]
lowIndices = filter <$> isLow <*> indices

isLow :: Matrix Int -> (Int, Int) -> Bool
isLow m pos = all (> x) (neighbours m pos)
  where x = m `at` pos

parse :: String -> Matrix Int
parse = fromList . map (map digitToInt) . lines
