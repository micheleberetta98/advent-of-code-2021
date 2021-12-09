module Matrix where

import           Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V

data Matrix a = Matrix (Int, Int) (Vector (Vector a))
  deriving (Show)

fromList :: [[a]] -> Matrix a
fromList lst = Matrix dims . V.fromList . map V.fromList $ lst
  where dims = (length lst, length (head lst))

dimensions :: Matrix a -> (Int, Int)
dimensions (Matrix d _) = d

at :: Matrix a -> (Int, Int) -> a
(Matrix _ m) `at` (i, j) = m ! i ! j

set :: (Int, Int) -> a -> Matrix a -> Matrix a
set (i, j) value (Matrix dims m) = Matrix dims $ m // [(i, updatedRow (m ! i))]
  where updatedRow v = v // [(j, value)]

neighbours :: Matrix a -> (Int, Int) -> [a]
neighbours m = map (m `at`) . neighbouringIndices m

neighbouringIndices :: Matrix a -> (Int, Int) -> [(Int, Int)]
neighbouringIndices (Matrix (rows, cols) _) (i, j) = filter valid [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]
  where valid (x, y) = x >= 0 && x < rows && y >= 0 && y < cols

indices :: Matrix a -> [(Int, Int)]
indices m = [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]
  where (rows, cols) = dimensions m

instance Functor Matrix where
  fmap f (Matrix dims m) = Matrix dims $ fmap (fmap f) m
