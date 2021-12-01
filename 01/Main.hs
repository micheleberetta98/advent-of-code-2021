module Main where

main :: IO ()
main = do
  print (countIncreased [199, 200, 208, 210, 200, 207, 240, 269, 260, 263])

countIncreased :: [Int] -> Int
countIncreased []     = 0
countIncreased (x:xs) = length $ filter increased $ zip (x:xs) xs
  where
    increased (x, y) = x < y
