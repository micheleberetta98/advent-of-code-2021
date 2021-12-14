module Main where

import           Data.Bifunctor
import           Data.List
import           Data.Map       (Map, (!))
import qualified Data.Map       as M

type Bag a = Map a Int
type Pair = (Char, Char)

main :: IO ()
main = do
  (polymer, rules) <- parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (solve 10 rules polymer)
  putStr "Answer 2:  " >> print (solve 40 rules polymer)

------------ Solutions

solve :: Int -> Map Pair Char -> String -> Int
solve n rules = tupleDiff . mostLeastCommon . steps n rules

mostLeastCommon :: Bag Char -> (Int, Int)
mostLeastCommon m = (mostCommon assocs, leastCommon assocs)
  where assocs = M.assocs m

steps :: Int -> Map Pair Char -> String -> Bag Char
steps n rules polymer = snd $ steps' n (initialPairs polymer, initialCount polymer)
  where
    steps' 0 acc = acc
    steps' k acc = steps' (k-1) (step rules acc)

initialPairs :: String -> Bag Pair
initialPairs polymer = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty $ pairs polymer

initialCount :: String -> Bag Char
initialCount = M.fromList . map ((,) <$> head <*> length) . group . sort

step :: Map Pair Char -> (Bag Pair, Bag Char) -> (Bag Pair, Bag Char)
step rules (pairNumbers, t) = (pairNumbers', addCounts t newLetters)
  where
    assocs = M.assocs pairNumbers
    newLetters = foldl' (\acc (k, v) -> M.insertWith (+) k v acc) M.empty $ map (first (rules !)) assocs
    x = concatMap (\(k, v) -> let x = rules ! k in [((fst k, x), v), ((x, snd k), v)]) assocs
    pairNumbers' = foldl' (\acc (k, v) -> M.insertWith (+) k v acc) M.empty x

addCounts :: Bag Char -> Bag Char -> Bag Char
addCounts base = foldl' (\acc (k, v) -> M.insertWith (+) k v acc) base . M.assocs

------------ Utils

tupleDiff :: (Int, Int) -> Int
tupleDiff (a, b) = a - b

mostCommon :: [(Char, Int)] -> Int
mostCommon = maximum . map snd

leastCommon :: [(Char, Int)] -> Int
leastCommon = minimum . map snd

pairs :: [a] -> [(a, a)]
pairs = map toTuple . filter ((>1) . length) . map (take 2) . tails
  where toTuple [a, b] = (a, b)

parse :: String -> (String, Map Pair Char)
parse s = (polymer, M.fromList (map (toTuple . words) rules))
  where
    (polymer:"":rules) = lines s
    toTuple [from, "->", to] = ((head from, head (tail from)), head to)
