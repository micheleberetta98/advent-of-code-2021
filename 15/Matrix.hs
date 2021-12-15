module Matrix where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.Vector  as V

type Pos = (Int, Int)
data SquareMatrix = SquareMatrix Int (V.Vector (V.Vector Int))

fromLists :: [[Int]] -> SquareMatrix
fromLists xs = let xs' = V.fromList (map V.fromList xs) in SquareMatrix (V.length xs') xs'

(<!>) :: SquareMatrix -> Pos -> Int
(<!>) (SquareMatrix d m) (i, j) =
  let (di, i') = i `quotRem` d
      (dj, j') = j `quotRem` d
      x = (m V.! i') V.! j' + (di + dj)
  in if x > 9 then x - 9 else x

neighbouringIndices :: Int -> SquareMatrix -> Pos -> HashSet Pos
neighbouringIndices k (SquareMatrix d _) (i, j) = HS.fromList $ filter valid [(i, j-1), (i, j+1), (i-1,j), (i+1,j)]
  where valid (x, y) = x >= 0 && x < k*d && y >= 0 && y < k*d
