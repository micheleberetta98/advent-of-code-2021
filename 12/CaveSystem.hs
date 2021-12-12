module CaveSystem where

import           Data.Char     (isUpper)
import           Data.Function (on)
import           Data.List     (foldl')
import           Data.Map      (Map, (!))
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)

------------ Types

data Cave
  = BigCave   { label :: String, neighbors :: [String] }
  | SmallCave { label :: String, neighbors :: [String] }
  | StartCave { label :: String, neighbors :: [String] }
  | EndCave   { label :: String, neighbors :: [String] }

type CaveSystem = Map String Cave

instance Eq Cave where
  (==) = (==) `on` label

instance Show Cave where
  show c = "Cave " ++ show (label c) ++ " [" ++ unwords (neighbors c) ++ "]"

instance Ord Cave where
  compare = compare `on` label

------------ System construction

cave :: String -> Cave
cave "start"      = StartCave "start" []
cave "end"        = EndCave "end" []
cave l
  | all isUpper l = BigCave l []
  | otherwise     = SmallCave l []

buildSystem :: [(String, String)] -> CaveSystem
buildSystem = foldl' addAssoc M.empty
  where
    addAssoc system (from, to) =
      let c1 = fromMaybe (cave from) $ M.lookup from system
          c2 = fromMaybe (cave to)   $ M.lookup to system
      in M.insert to (addNeighbor c1 c2) $ M.insert from (addNeighbor c2 c1) system

startCave :: CaveSystem -> Cave
startCave = (! "start")

addNeighbor :: Cave -> Cave -> Cave
addNeighbor n cave = cave{ neighbors = label n : neighbors cave }

getNeighbors :: CaveSystem -> Cave -> [Cave]
getNeighbors system = map (system !) . neighbors
