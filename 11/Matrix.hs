module Matrix where

import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Vector (Vector, (//))
import qualified Data.Vector as V

data Matrix a = Matrix (Int, Int) (Vector (Vector a))
  deriving (Eq)

instance Functor Matrix where
  fmap f (Matrix dims m) = Matrix dims $ fmap (fmap f) m

fromLists :: [[a]] -> Matrix a
fromLists lst = Matrix dims . V.fromList . map V.fromList $ lst
  where dims = (length lst, length (head lst))

(!) :: Matrix a -> (Int, Int) -> a
(Matrix _ m) ! (i, j) = m V.! i V.! j

modifyElem :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
modifyElem f (i, j) (Matrix dims m) = Matrix dims $ m // [(i, updatedRow (m V.! i))]
  where updatedRow v = v // [(j, f (v V.! j))]

neighbouringIndices :: Matrix a -> (Int, Int) -> [(Int, Int)]
neighbouringIndices (Matrix (rows, cols) _) (i, j) = filter valid ijs
  where
    ijs = [(x, y) | x <- [i-1..i+1], y <- [j-1..j+1], (x,y) /= (i,j) ]
    valid (x, y) = x >= 0 && x < rows && y >= 0 && y < cols

indices :: Matrix a -> Set (Int, Int)
indices (Matrix (nrows, ncols) _) = S.fromList [(i, j) | i <- [0..nrows-1], j <- [0..ncols-1]]

countIf :: (a -> Bool) -> Matrix a -> Int
countIf p = length . indicesWhere p

indicesWhere :: (a -> Bool) -> Matrix a -> Set (Int, Int)
indicesWhere p m = S.filter (p . (m !)) $ indices m
