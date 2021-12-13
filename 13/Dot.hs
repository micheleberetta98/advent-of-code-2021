module Dot where

import           Data.Bifunctor (first, second)
import           Data.Set       (Set)
import qualified Data.Set       as S

type Dots = Set Dot
type Dot = (Int, Int)

data Fold = XFold Int | YFold Int

foldAlong :: Fold -> Dots -> Dots
foldAlong (XFold x) = S.map (first  (newCoord x))
foldAlong (YFold y) = S.map (second (newCoord y))

newCoord :: Int -> Int -> Int
newCoord middlePoint initial
  | initial > middlePoint = middlePoint - (initial - middlePoint)
  | otherwise             = initial

dotsMatrix :: Set Dot -> String
dotsMatrix dots = unlines [[showCoords (x, y) | x <- [0..rows]] | y <- [0..cols]]
  where
    showCoords (i, j) = if (i, j) `S.member` dots then '#' else '.'
    (rows, cols) = S.findMax dots
