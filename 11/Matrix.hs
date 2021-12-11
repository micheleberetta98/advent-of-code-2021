module Matrix where

import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Vector (Vector, (//))
import qualified Data.Vector as V

-- | A 10x10 matrix
newtype Matrix a = Matrix (Vector (Vector a))
  deriving (Eq)

instance Functor Matrix where
  fmap f (Matrix m) = Matrix $ fmap (fmap f) m

rows, cols :: Int
(rows, cols) = (10, 10)

fromLists :: [[a]] -> Matrix a
fromLists = Matrix . V.fromList . map V.fromList

(!) :: Matrix a -> (Int, Int) -> a
(Matrix m) ! (i, j) = m V.! i V.! j

modifyElem :: (a -> a) -> Matrix a -> (Int, Int) -> Matrix a
modifyElem f (Matrix m) (i, j) = Matrix $ m // [(i, updatedRow (m V.! i))]
  where updatedRow v = v // [(j, f (v V.! j))]

neighbouringIndices :: (Int, Int) -> [(Int, Int)]
neighbouringIndices (i, j) = filter valid ijs
  where
    ijs = [(x, y) | x <- [i-1..i+1], y <- [j-1..j+1], (x,y) /= (i,j) ]
    valid (x, y) = x >= 0 && x < rows && y >= 0 && y < cols

countIf :: (a -> Bool) -> Matrix a -> Int
countIf p = length . indicesWhere p

indicesWhere :: (a -> Bool) -> Matrix a -> Set (Int, Int)
indicesWhere p m = S.filter (p . (m !)) indices

indices :: Set (Int, Int)
indices = S.fromList [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]
