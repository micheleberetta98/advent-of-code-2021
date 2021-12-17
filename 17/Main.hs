{-# LANGUAGE TransformListComp #-}
module Main where

import           Data.Function
import           Data.Ix
import           Data.List
import           Text.Parsec
import           Text.Parsec.String (Parser)

type Pos = (Int, Int)
type Vel = (Int, Int)
type Bounds = (Pos, Pos)

main :: IO ()
main = do
  Right bounds@((x0, y0), (x1, y1)) <- parse targetArea "" <$> readFile "input.txt"
  let vs = yVelocities bounds
  putStr "Answer 1:  " >> print (maximum vs)
  putStr "Answer 2:  " >> print (length vs)

yVelocities :: ((Int, Int), (Int, Int)) -> [Int]
yVelocities bounds@((x0, y0), (x1, y1)) = do
  -- In order to arrive at the platform, given that vx decreases by 1 at each step,
  -- it is needed that
  --    sumOfFirstN(vx) >= x0     sumOfFirstN(vx) is the sum from 1 to vx, which is also the finalX reached
  --    vx(vx+1)/2 >= x0
  --    vx^2 + vx >= 2 x0
  --    vx^2 + vx + 1/4 >= 2 x0 + 1/4
  --    (vx+1/2)^2 >= 2 x0 + 1/4
  --    vx >= √(2 x0 + 1/4) - 1/2 = vx0
  -- So possible values for vx go from vx0 to x1 (just to have less steps)
  let vx0 = (ceiling $ sqrt (2 * fromIntegral x0 + 0.25) - 0.5) :: Int
  vx <- [vx0 .. x1]
  let xs = scanl' (+) 0 $ [vx, vx-1 .. 1] ++ repeat 0
  let xf = vx * (vx + 1) `div` 2
  [ maximum ys
    | vy <- [y0..]
    , let ys = takeWhile (>= y0) $ scanl' (+) 0 [vy, vy - 1..]
    , let yf = vy * (vx + 1) - xf
    , then takeWhile by yf <= y1 || xf <= x1 && vy <= abs y0
    , any (inRange bounds) $ zip xs ys
    ]

targetArea :: Parser Bounds
targetArea = do
  x0 <- string "target area: x=" *> number
  x1 <- string ".." *> number
  y0 <- string ", y=" *> number
  y1 <- string ".." *> number
  pure ((x0, y0), (x1, y1))
    where
      number = sign <*> (read <$> many1 digit)
      sign = (negate <$ char '-') <|> pure id
