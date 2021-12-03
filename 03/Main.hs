{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import           Data.Char
import           Data.Function
import           Data.List

main :: IO ()
main = do
  binarynums <- lines <$> readFile "input.txt"
  let (g, e) = rates binarynums
  print (toDec g  * toDec e)
  let o = oxygenRating binarynums
      c = co2rating binarynums
  print (toDec o * toDec c)

rates :: [String] -> (String, String)
rates nums = (gamma, eps)
  where
    gamma = map mostCommon $ transpose nums
    eps = map leastCommon $ transpose nums

oxygenRating :: [String] -> String
oxygenRating = findRating mostCommon

co2rating :: [String] -> String
co2rating = findRating leastCommon

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (compare `on` length) . group . sort

findRating :: (String -> Char) -> [String] -> String
findRating f = findRating' f . map (\x -> (x, x))
  where
    findRating' :: (String -> Char) -> [(String, String)] -> String
    findRating' _ [b]  = fst b
    findRating' f bits = findRating' f $ map (fmap tail) $ filter (\(x, y) -> head y == common) bits
      where
        numsToKeep = filter (\x -> head (snd x) == common) bits
        common = f (map (head . snd) bits)


toDec :: String -> Int
toDec = foldl' (\acc x -> 2 * acc + digitToInt x) 0
