module Main where

import           Data.List
import           Line

main :: IO ()
main = do
  Right ls <- parseLines <$> readFile "input.txt"
  putStr "Solution: " >> print (countAtLeast 2 ls)

countAtLeast :: Int -> [Line] -> Int
countAtLeast k = length . filter (atLeast k) . group . sort . concat
  where atLeast l xs = length xs >= l
