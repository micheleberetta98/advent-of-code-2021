module Main where

import           Data.Bifunctor
import           Data.List
import           Data.Semigroup hiding (diff)

main :: IO ()
main = do
  nums <- parse <$> readFile "input.txt"
  putStr "Part 1:   " >> print (lowestFuel fuelCost nums)
  putStr "Part 2:   " >> print (lowestFuel fuelCost' nums)

lowestFuel :: ([Int] -> Int -> Int) -> [Int] -> Int
lowestFuel f nums = minimum $ map (f nums) [low..high]
  where
    (low, high) = bimap getMin getMax $ foldMap (\x -> (Min x, Max x)) nums

------------ Functions

fuelCost :: [Int] -> Int -> Int
fuelCost xs m = sum $ map (diff m) xs

fuelCost' :: [Int] -> Int -> Int
fuelCost' xs m = sum $ map (sumOfFirstN . diff m) xs
  where sumOfFirstN n = (n * (n + 1)) `div` 2

diff :: Num a => a -> a -> a
diff x y = abs (x - y)

------------ Parsing input

parse :: String -> [Int]
parse = map read . splitOn ','

splitOn :: Char -> String -> [String]
splitOn c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> t : splitOn c s''
      where (t, s'') = break (== c) s'
