module Main where

import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.List

main :: IO ()
main = do
  nums <- lines <$> readFile "input.txt"
  let gamma = rate mostCommon nums
      eps   = rate leastCommon nums
  putStr "Gamma rating:  " >> print gamma
  putStr "Eps rating:    " >> print eps
  putStr "Answer 1:      " >> print (gamma * eps)
  putStrLn ""
  let oxygen = findRating mostCommon nums
      co2    = findRating leastCommon nums
  putStr "Oxygen rating: " >> print oxygen
  putStr "CO2 rating:    " >> print co2
  putStr "Answer 2:      " >> print (oxygen * co2)

rate :: (String -> Char) -> [String] -> Int
rate f = toDec . map f . transpose

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (compare `on` length) . group . sort

findRating :: (String -> Char) -> [String] -> Int
findRating f = toDec . findRating' f . map (\x -> (x, x))
  where
    findRating' _ [b]  = fst b
    findRating' f bits = findRating' f $ map discardFirstBit $ filter (beginsWith common) bits
      where
        discardFirstBit = second tail
        beginsWith k x = head (snd x) == k
        common = f (map (head . snd) bits)

toDec :: String -> Int
toDec = foldl' (\acc x -> 2 * acc + digitToInt x) 0
