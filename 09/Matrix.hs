module Matrix where

import           Data.Vector (Vector, (//))
import qualified Data.Vector as V

data Matrix a = Matrix (Int, Int) (Vector (Vector a))

instance Functor Matrix where
  fmap f (Matrix dims m) = Matrix dims $ fmap (fmap f) m

nrows, ncols :: Matrix a -> Int
nrows (Matrix (r, _) _) = r
ncols (Matrix (_, c) _) = c

fromLists :: [[a]] -> Matrix a
fromLists lst = Matrix dims . V.fromList . map V.fromList $ lst
  where dims = (length lst, length (head lst))

(!) :: Matrix a -> (Int, Int) -> a
(Matrix _ m) ! (i, j) = m V.! i V.! j

setElem :: a -> (Int, Int) -> Matrix a -> Matrix a
setElem value (i, j) (Matrix dims m) = Matrix dims $ m // [(i, updatedRow (m V.! i))]
  where updatedRow v = v // [(j, value)]

neighbouringIndices :: Matrix a -> (Int, Int) -> [(Int, Int)]
neighbouringIndices (Matrix (rows, cols) _) (i, j) = filter valid [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]
  where valid (x, y) = x >= 0 && x < rows && y >= 0 && y < cols

indices :: Matrix a -> [(Int, Int)]
indices m = [(i, j) | i <- [0..nrows m-1], j <- [0..ncols m-1]]
