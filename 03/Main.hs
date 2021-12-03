module Main where

import           Data.Char
import           Data.Function
import           Data.List

main :: IO ()
main = do
  binarynums <- lines <$> readFile "input.txt"
  let (g, e) = rates binarynums
  print (toDec g  * toDec e)

rates :: [String] -> (String, String)
rates nums = (gamma, eps)
  where
    gamma = map mostCommon $ transpose nums
    eps = map invert gamma

mostCommon :: [Char] -> Char
mostCommon = head . maximumBy (compare `on` length) . group . sort

invert :: Char -> Char
invert '0' = '1'
invert '1' = '0'
invert x   = x -- For completeness

toDec :: String -> Int
toDec = foldl' (\acc x -> 2 * acc + digitToInt x) 0
