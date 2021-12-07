module Main where

import           Data.List

main :: IO ()
main = do
  nums <- parse <$> readFile "input.txt"
  putStr "Part 1:   " >> print (lowestFuel nums)
  pure ()

------------ Part 1

lowestFuel :: [Int] -> Int
lowestFuel nums = minimum $ map (fuelCost nums) minMaxInterval
  where
    (low, high) = minmax nums
    minMaxInterval = [low..high]

fuelCost :: [Int] -> Int -> Int
fuelCost xs m = sum $ map (\x -> abs (x - m)) xs

------------ Utils

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
