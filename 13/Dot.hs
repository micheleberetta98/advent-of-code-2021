module Dot where

import           Data.Set (Set)
import qualified Data.Set as S

type Dots = Set Dot

newtype Dot = Dot { coords :: (Int, Int) }
  deriving (Eq, Ord)

data Fold = XFold Int | YFold Int

foldAlong :: Fold -> Dots -> Dots
foldAlong (XFold x) = S.map (foldDotOnX x)
foldAlong (YFold y) = S.map (foldDotOnY y)

foldDotOnX, foldDotOnY :: Int -> Dot -> Dot
foldDotOnX v (Dot (x, y)) = Dot (newCoord x v, y)
foldDotOnY v (Dot (x, y)) = Dot (x, newCoord y v)

newCoord :: Int -> Int -> Int
newCoord initial middlePoint
  | initial > middlePoint = middlePoint - (initial - middlePoint)
  | otherwise             = initial

dotsMatrix :: Set Dot -> String
dotsMatrix dots = unlines [[showCoords (x, y) | x <- [0..rows]] | y <- [0..cols]]
  where
    showCoords (i, j) = if Dot (i, j) `S.member` dots then '#' else '.'
    rows = S.findMax (S.map (fst . coords) dots)
    cols = S.findMax (S.map (snd . coords) dots)
