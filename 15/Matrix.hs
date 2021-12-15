module Matrix where

import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Vector (Vector, (//))
import qualified Data.Vector as V

data Matrix a = Matrix (Int, Int) (Vector (Vector a))
  deriving (Eq, Show)

instance Functor Matrix where
  fmap f (Matrix d m) = Matrix d $ fmap (fmap f) m

type Pos = (Int, Int)

rows, cols :: Matrix a -> Int
rows (Matrix (r, _) _) = r
cols (Matrix (_, c) _) = c

fromLists :: [[a]] -> Matrix a
fromLists xs = Matrix (r, c) . V.fromList . map V.fromList $ xs
  where (r, c) = (length xs, length (head xs))

(!) :: Matrix a -> Pos -> a
(Matrix _ m) ! (i, j) = m V.! i V.! j

setElem :: b -> Pos -> Matrix b -> Matrix b
setElem x = flip $ modifyElem (const x)

modifyElem :: (a -> a) -> Matrix a -> Pos -> Matrix a
modifyElem f (Matrix d m) (i, j) = Matrix d $ m // [(i, updatedRow (m V.! i))]
  where updatedRow v = v // [(j, f (v V.! j))]

neighbouringIndices :: Matrix a -> Pos -> [Pos]
-- neighbouringIndices m (i, j) = filter valid [(i, j-1), (i, j+1), (i-1,j), (i+1,j)]
neighbouringIndices m (i, j) = filter valid [(i, j+1), (i+1,j)]
  where valid (x, y) = x >= 0 && x < rows m && y >= 0 && y < cols m

countIf :: (a -> Bool) -> Matrix a -> Int
countIf p = length . indicesWhere p

indicesWhere :: (a -> Bool) -> Matrix a -> Set Pos
indicesWhere p m = S.filter (p . (m !)) (indices m)

indices :: Matrix a -> Set Pos
indices (Matrix (rows, cols) _) = S.fromList [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]
