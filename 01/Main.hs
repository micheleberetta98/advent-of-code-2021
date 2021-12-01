module Main where

main :: IO ()
main = do
  s <- readFile "input.txt"
  let nums = map read (lines s)
  print (countIncreased nums)


countIncreased :: [Int] -> Int
countIncreased []     = 0
countIncreased (x:xs) = length $ filter increased $ zip (x:xs) xs
  where
    increased (x, y) = x < y
