{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Arrow   ((&&&))
import           Data.Bifunctor  (Bifunctor (bimap, first))
import           Data.List
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import           Data.Maybe

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
solve n rules = tupleDiff . (maximum &&& minimum) . M.elems . steps n rules
  where tupleDiff (a, b) = a - b

steps :: Int -> Rules -> String -> Bag Char
steps n rules polymer = snd $ steps' n (initialPairs polymer, initialCount polymer)
  where
    steps' 0 acc = acc
    steps' k acc = steps' (k-1) (step rules acc)

initialPairs :: String -> Bag Pair
initialPairs polymer = toBag $ zip (pairs polymer) (repeat 1)

initialCount :: String -> Bag Char
initialCount = M.fromList . map ((,) <$> head <*> length) . group . sort

step :: Rules -> (Bag Pair, Bag Char) -> (Bag Pair, Bag Char)
step rules (pairNumbers, letterCounts) = (pairNumbers', M.unionsWith (+) [newLetters, letterCounts])
  where
    newPairs = map (deriveNewPairsFrom rules) $ M.assocs pairNumbers
    newLetters = toBag $ map (first snd . fst) newPairs
    pairNumbers' = toBag (concatPairs newPairs)

------------ Utils

concatPairs :: [(a, a)] -> [a]
concatPairs (p:rest) = fst p : snd p : concatPairs rest
concatPairs _        = []

deriveNewPairsFrom :: Rules -> (Pair, Int) -> ((Pair, Int), (Pair, Int))
deriveNewPairsFrom rules (k, v) = bimap (,v) (,v) ((fst k, x), (x, snd k))
  where x = rules ! k

toBag :: Ord a => [(a, Int)] -> Bag a
toBag = M.fromListWith (+)

pairs :: [a] -> [(a, a)]
pairs = mapMaybe (toTuple . take 2) . tails
  where
    toTuple [a, b] = Just (a, b)
    toTuple _      = Nothing

parse :: String -> (String, Rules)
parse s = (polymer, M.fromList (mapMaybe (toTuple . words) rules))
  where
    (polymer:"":rules) = lines s
    toTuple [from, "->", to] = Just ((head from, head (tail from)), head to)
    toTuple _                = Nothing
