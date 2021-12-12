module Main where

import           CaveSystem
import           Data.List
import           Data.Map   (Map, (!?))
import qualified Data.Map   as M
import           Data.Maybe

main :: IO ()
main = do
  system <- buildSystem . parse <$> readFile "input.txt"
  putStr "Answer 1: " >> print (length $ allPathsWith (M.member, visit1)       system)
  putStr "Answer 2: " >> print (length $ allPathsWith (isVisited2, visit2) system)

------------ Solutions

type Seen = Map Cave Int
type IsVisitedFn = Cave -> Seen -> Bool
type VisitFn = Cave -> Seen -> Seen

allPathsWith :: (IsVisitedFn, VisitFn) -> CaveSystem -> [[Cave]]
allPathsWith (isVisited, visit) system = allPaths' M.empty (startCave system)
  where
    allPaths' _ c@(EndCave _ _) = [[c]]
    allPaths' seen cave
      | isVisited cave seen = []
      | otherwise           = map (cave :) $ concatMap (allPaths' (visit cave seen)) (getNeighbors system cave)

visit1 :: VisitFn
visit1 (BigCave _ _) s = s
visit1 cave          s = M.insertWith (+) cave 1 s

isVisited2 :: IsVisitedFn
isVisited2 cave@(BigCave _ _) _      = False
isVisited2 cave@(SmallCave _ _) seen = visits == 2 || (visits == 1 && doubleVisitedSmallCave seen)
  where visits = fromMaybe 0 (seen !? cave)
isVisited2 cave seen                 = cave `M.member` seen

visit2 :: VisitFn
visit2 cave@(SmallCave _ _) seen
  | visits == 0                 = M.insertWith (+) cave 1 seen
  | doubleVisitedSmallCave seen = seen
  | otherwise                   = M.insertWith (+) cave 1 seen
  where visits = fromMaybe 0 (seen !? cave)
visit2 c s                      = visit1 c s

doubleVisitedSmallCave :: Map Cave Int -> Bool
doubleVisitedSmallCave =  any isSmall . M.keys . M.filter (> 1)

------------ Parsing

parse :: String -> [([Char], [Char])]
parse = map split . lines
  where split = fmap tail . break (== '-')
