module Main where

import           Control.Monad.State
import           Data.List
import           Data.Maybe

data Bracket = Round | Square | Curly | Angular
  deriving (Show, Eq)

data Symbol = Open Bracket | Closed Bracket
  deriving (Show, Eq)

data Result = Corrupted { corruptionBracket :: Bracket } | NotCorrupted
  deriving (Show, Eq)

main :: IO ()
main = do
  chunks <- map (map parse) . lines <$> readFile "input.txt"
  let results = map corrupted chunks
  putStr "Answer 1:  " >> print (errorScore results)
  putStr "Answer 2:  " >> print (completionScore results)

------------ Solutions

errorScore :: [(Result, [Bracket])] -> Int
errorScore = sum . map (corruptionPoints . corruptionBracket . fst) . filter isCorrupted

completionScore :: [(Result, [Bracket])] -> Int
completionScore = middleElement . sort . completionScores
  where middleElement xs = xs !! (length xs `div` 2)

completionScores :: [(Result, [Bracket])] -> [Int]
completionScores = map (getPoints . snd) . filter (not . isCorrupted)
  where getPoints = foldl' (\acc x -> 5 * acc + completionPoints x) 0

------------ Utils

isCorrupted :: (Result, [Bracket]) -> Bool
isCorrupted (Corrupted _, _) = True
isCorrupted _                = False

corrupted :: [Symbol] -> (Result, [Bracket])
corrupted xs = runState (corrupted' xs) []
  where
    corrupted' :: [Symbol] -> State [Bracket] Result
    corrupted' []                = pure NotCorrupted
    corrupted' (Open o : rest)   = modify (o :) >> corrupted' rest
    corrupted' (Closed c : rest) = do
      current <- gets listToMaybe
      if current == Just c
        then modify tail >> corrupted' rest
        else pure (Corrupted c)

corruptionPoints :: Bracket -> Int
corruptionPoints Round   = 3
corruptionPoints Square  = 57
corruptionPoints Curly   = 1197
corruptionPoints Angular = 25137

completionPoints :: Bracket -> Int
completionPoints Round   = 1
completionPoints Square  = 2
completionPoints Curly   = 3
completionPoints Angular = 4

------------ Parsing

parse :: Char -> Symbol
parse '(' = Open Round
parse '[' = Open Square
parse '{' = Open Curly
parse '<' = Open Angular
parse ')' = Closed Round
parse ']' = Closed Square
parse '}' = Closed Curly
parse '>' = Closed Angular
parse _   = undefined -- Input is supposed correct
