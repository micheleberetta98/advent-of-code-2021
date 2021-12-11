module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Matrix
import           Octopus

main :: IO ()
main = do
  m <- parse <$> readFile "input.txt"
  let steps = evalState (sequence $ repeat step) m
  putStr "Answer 1:  " >> print (flashesAfter 100 steps)
  putStr "Answer 2:  " >> print (firstStepAllFlashed steps)

------------ Solutions

flashesAfter :: Int -> [Int] -> Int
flashesAfter x = sum . take x

firstStepAllFlashed :: [Int] -> Maybe Int
firstStepAllFlashed = fmap (+1) . elemIndex 100

------------ Utils

step :: State (Matrix Octopus) Int
step =
  modify' (fmap increaseLuminosity)
  >> gets flashedIndices >>= flash
  >> gets (countIf flashed) <* modify' (fmap resetOctopus)

flash :: Set (Int, Int) -> State (Matrix Octopus) ()
flash ixs = do
  m <- get
  let m' = foldl' (modifyElem increaseLuminosity) m $ concatMap neighbouringIndices ixs
  put m'
  when (m /= m') $ do
    flash $ flashedIndices m' `S.difference` flashedIndices m

flashedIndices :: Matrix Octopus -> Set (Int, Int)
flashedIndices = indicesWhere flashed

parse :: String -> Matrix Octopus
parse = fromLists . map (map (Octopus . digitToInt)) . lines
