module Main where

import           CaveSystem
import           Data.List

main :: IO ()
main = do
  system <- buildSystem . parse <$> readFile "input.txt"
  putStr "Answer 1: " >> print (length $ allPathsWith (elem, visit1)       system)
  putStr "Answer 2: " >> print (length $ allPathsWith (isVisited2, visit2) system)

------------ Solutions

type IsVisitedFn = Cave -> [Cave] -> Bool
type VisitFn = Cave -> [Cave] -> [Cave]

allPathsWith :: (IsVisitedFn, VisitFn) -> CaveSystem -> [[Cave]]
allPathsWith (isVisited, visit) system = allPaths' [] (startCave system)
  where
    allPaths' _ c@(EndCave _ _) = [[c]]
    allPaths' seen cave
      | isVisited cave seen = []
      | otherwise           = map (cave :) $ concatMap (allPaths' (visit cave seen)) (getNeighbors system cave)

visit1 :: VisitFn
visit1 (BigCave _ _) s = s
visit1 cave          s = if cave `elem` s then s else cave : s

isVisited2 :: IsVisitedFn
isVisited2 cave@(BigCave _ _) seen        = False
isVisited2 cave@(SmallCave _ _) seen = not (null visits) && doubleVisitedSmallCave seen
  where visits = filter (== cave) seen
isVisited2 cave seen                 = cave `elem` seen

visit2 :: VisitFn
visit2 cave@(SmallCave _ _) seen
  | null visits                 = cave : seen
  | doubleVisitedSmallCave seen = seen
  | otherwise                   = cave : seen
  where visits = filter (== cave) seen
visit2 c s = visit1 c s

doubleVisitedSmallCave :: [Cave] -> Bool
doubleVisitedSmallCave = any ((>1) . length) . group . sort . filter isSmall

------------ Parsing

parse :: String -> [([Char], [Char])]
parse = map split . lines
  where split = fmap tail . break (== '-')
