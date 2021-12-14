module Main where

import           Control.Arrow   ((&&&))
import           Data.Bifunctor  (Bifunctor (first))
import           Data.List       (foldl', tails)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)

type Pair = (Char, Char)
type Rules = Map Pair Char
type Bag a = Map a Int

main :: IO ()
main = do
  (polymer, rules) <- parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (solve 10 rules polymer)
  putStr "Answer 2:  " >> print (solve 40 rules polymer)

------------ Solutions

solve :: Int -> Rules -> String -> Int
solve n rules = tupleDiff . (maximum &&& minimum) . M.elems . afterSteps n rules
  where tupleDiff (a, b) = a - b

afterSteps :: Int -> Rules -> String -> Bag Char
afterSteps n rules polymer = snd $ afterSteps' n (toBag (pairs polymer), toBag polymer)
  where
    afterSteps' 0 acc = acc
    afterSteps' k acc = afterSteps' (k-1) (step rules acc)

step :: Rules -> (Bag Pair, Bag Char) -> (Bag Pair, Bag Char)
step rules (pairNumbers, letterCounts) =
  ( bagFromPairs (concatPairs newPairs)
  , M.unionWith (+) newLetters letterCounts
  )
  where
    newPairs = map (deriveNewPairsFrom rules) $ M.assocs pairNumbers
    newLetters = bagFromPairs $ map (first snd . fst) newPairs

------------ Utils

concatPairs :: [(a, a)] -> [a]
concatPairs = foldl' (\acc p -> fst p : snd p : acc) []

deriveNewPairsFrom :: Rules -> (Pair, Int) -> ((Pair, Int), (Pair, Int))
deriveNewPairsFrom rules (k@(a, b), v) =
  ( ((a, x), v)
  , ((x, b), v)
  )
  where x = rules ! k

toBag :: Ord a => [a] -> Bag a
toBag = bagFromPairs . flip zip (repeat 1)

bagFromPairs :: Ord a => [(a, Int)] -> Bag a
bagFromPairs = M.fromListWith (+)

pairs :: [a] -> [(a, a)]
pairs = mapMaybe (toTuple . take 2) . tails
  where
    toTuple [a, b] = Just (a, b)
    toTuple _      = Nothing

parse :: String -> (String, Rules)
parse s = (polymer, M.fromList (mapMaybe (toTuple . words) rules))
  where
    (polymer:"":rules) = lines s
    toTuple [a:b:_, "->", to:_] = Just ((a, b), to)
    toTuple _                   = Nothing
