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
step = do
  modify' (fmap increaseLuminosity)
  gets (indicesWhere flashed) >>= flash
  x <- gets (countIf flashed)
  modify' (fmap resetOctopus)
  pure x

flash :: Set (Int, Int) -> State (Matrix Octopus) ()
flash flashedIxs = do
  os <- get
  let os' = foldl' (modifyElem increaseLuminosity) os $ concatMap neighbouringIndices flashedIxs
  put os'
  when (os /= os') $ do
    flash $ indicesWhere flashed os' `S.difference` indicesWhere flashed os

parse :: String -> Matrix Octopus
parse = fromLists . map (map (Octopus . digitToInt)) . lines
