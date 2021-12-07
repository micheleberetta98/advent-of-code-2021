module Main where

import           Data.List

main :: IO ()
main = do
  nums <- parse <$> readFile "input.txt"
  putStr "Part 1:   " >> print (lowestFuel fuelCost nums)
  putStr "Part 2:   " >> print (lowestFuel fuelCost' nums)

lowestFuel :: ([Int] -> Int -> Int) -> [Int] -> Int
lowestFuel f nums = minimum $ map (f nums) minMaxInterval
  where
    (low, high) = minmax nums
    minMaxInterval = [low..high]

------------ Functions

fuelCost :: [Int] -> Int -> Int
fuelCost xs m = sum $ map (diff m) xs

fuelCost' :: [Int] -> Int -> Int
fuelCost' xs m = sum $ map (sumOfFirstN . diff m) xs
  where sumOfFirstN n = (n * (n + 1)) `div` 2

diff :: Num a => a -> a -> a
diff x y = abs (x - y)

minmax :: Ord a => [a] -> (a, a)
minmax = (,) <$> minimum <*> maximum

------------ Parsing input

parse :: String -> [Int]
parse = map read . splitOn ','

splitOn :: Char -> String -> [String]
splitOn c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> t : splitOn c s''
      where (t, s'') = break (== c) s'
