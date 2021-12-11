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
  putStr "Answer 1:  " >> print (flashesAfter 100 m)
  putStr "Answer 2:  " >> print (firstStepAllFlashed m)

------------ Solutions

flashesAfter :: Int -> Matrix Octopus -> Int
flashesAfter x = sum . evalState (replicateM x step)

firstStepAllFlashed :: Matrix Octopus -> Maybe Int
firstStepAllFlashed = fmap (+1) . elemIndex 100 . evalState (sequence $ repeat step)

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
  let os' = applyFlashEffect os $ concatMap (neighbouringIndices os) flashedIxs
      willFlashIxs = indicesWhere flashed os' `S.difference` indicesWhere flashed os
  put os'
  when (os /= os') (flash willFlashIxs)
  where
    applyFlashEffect = foldl' (flip (modifyElem increaseLuminosity))

parse :: String -> Matrix Octopus
parse = fromLists . map (map (Octopus . digitToInt)) . lines
