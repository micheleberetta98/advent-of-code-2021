module Main where

import           Lanternfish

main :: IO ()
main = do
  fish <- parse <$> readFile "input.txt"
  putStr "Answer 1:   " >> print (fishAtDay 80 (days fish))

------------ Functions

fishAtDay :: Int -> [[Lanternfish]] -> Int
fishAtDay n lst = length (lst !! n)

days :: [Lanternfish] -> [[Lanternfish]]
days = iterate (concatMap spawn)

------------ Parsing

parse :: String -> [Lanternfish]
parse = map (Lanternfish . read) . splitOn ','

splitOn :: Char -> String -> [String]
splitOn c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> t : splitOn c s''
      where (t, s'') = break (== c) s'
