module Main where

import           CaveSystem
import           Data.Set   (Set, empty, insert, member)

main :: IO ()
main = do
  system <- buildSystem . parse <$> readFile "input.txt"
  putStr "Answer 1: " >> print (length $ allPaths system)

------------ Solutions

allPaths :: CaveSystem -> [[Cave]]
allPaths system = allPaths' empty (startCave system)
  where
    allPaths' _ c@(EndCave _ _) = [[c]]
    allPaths' seen cave
      | cave `member` seen   = []
      | otherwise              = map (cave :) $ concatMap (allPaths' (visit cave seen)) (getNeighbors system cave)

visit :: Cave -> Set Cave -> Set Cave
visit (BigCave _ _) s = s
visit cave          s = insert cave s

------------ Parsing

parse :: String -> [([Char], [Char])]
parse = map split . lines
  where split = fmap tail . break (== '-')
