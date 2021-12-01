module Main where

main :: IO ()
main = do
  s <- readFile "input.txt"
  let nums = map read (lines s)
  print (countIncreased $ sum3by3 nums)

countIncreased :: [Int] -> Int
countIncreased []     = 0
countIncreased (x:xs) = length $ filter increased $ zip (x:xs) xs
  where
    increased (x, y) = x < y

sum3by3 :: [Int] -> [Int]
sum3by3 (x:y:z:xs) = (x + y + z) : sum3by3 (y:z:xs)
sum3by3 _          = []
