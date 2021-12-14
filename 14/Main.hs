module Main where

import           Data.List
import           Data.Map  (Map, (!))
import qualified Data.Map  as M

main :: IO ()
main = do
  (polymer, rules) <- parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (answer1 rules polymer)

------------ Solutions

answer1 :: Map String Char -> [Char] -> Int
answer1 rules polymer = most - least
  where
    (most, least) = mostLeastCommon $ steps rules polymer !! 10

------------ Utils

mostLeastCommon :: Ord a => [a] -> (Int, Int)
mostLeastCommon = mostLeast . map length . group . sort
  where mostLeast xs = (maximum xs, minimum xs)

steps :: Map String Char -> String -> [String]
steps rules = iterate' (step rules)

step :: Map String Char -> String -> String
step rules = init . concat . (insertedLetters >>= zipWith (\a b -> [b, a]))
  where
    insertedLetters = (++ " ") . map (rules !) . pairs
    pairs (a:b:rest) = [a, b] : pairs (b : rest)
    pairs _          = []


parse :: String -> (String, Map String Char)
parse s = (polymer, M.fromList (map (toTuple . words) rules))
  where
    (polymer:"":rules) = lines s
    toTuple [from, "->", to] = (from, head to)
    toTuple _                = ("", ' ')
