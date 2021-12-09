module Matrix where

import           Data.Maybe
import           Data.Vector (Vector, (!?))
import qualified Data.Vector as V

data Matrix a = Matrix (Int, Int) (Vector (Vector a))

fromList :: [[a]] -> Matrix a
fromList lst = Matrix dims . V.fromList . map V.fromList $ lst
  where dims = (length lst, length (head lst))

dimensions :: Matrix a -> (Int, Int)
dimensions (Matrix d _) = d

at :: Matrix a -> (Int, Int) -> Maybe a
(Matrix _ m) `at` (i, j) = m !? i >>= (!? j)

neighbours :: Matrix a -> (Int, Int) -> [a]
neighbours m (i, j) = catMaybes neighbours'
  where neighbours' =
          [ m `at` (i-1, j)
          , m `at` (i+1, j)
          , m `at` (i, j-1)
          , m `at` (i, j+1)
          ]
