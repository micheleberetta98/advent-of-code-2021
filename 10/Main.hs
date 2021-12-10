module Main where

import           Control.Monad.State
import           Data.List
import           Data.Maybe

data Bracket = Round | Square | Curly | Angular
  deriving (Show, Eq)

data Symbol = Open Bracket | Closed Bracket
  deriving (Show, Eq)

data Result = Corrupted { corruptionBracket :: Bracket } | NotCorrupted

main :: IO ()
main = do
  chunks <- map parse . lines <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (errorScore chunks)

------------ Solutions

errorScore :: [[Symbol]] -> Int
errorScore = sum . map (corruptionPoints . corruptionBracket) . filter isCorrupted . map evalCorrupted

------------ Utils

evalCorrupted :: [Symbol] -> Result
evalCorrupted x = evalState (corrupted x) []

isCorrupted :: Result -> Bool
isCorrupted (Corrupted _) = True
isCorrupted _             = False

corrupted :: [Symbol] -> State [Bracket] Result
corrupted []              = pure NotCorrupted
corrupted (Open o : xs)   = modify (o :) >> corrupted xs
corrupted (Closed c : xs) = do
  current <- gets listToMaybe
  if current == Just c
    then modify tail >> corrupted xs
    else pure (Corrupted c)

corruptionPoints :: Bracket -> Int
corruptionPoints Round   = 3
corruptionPoints Square  = 57
corruptionPoints Curly   = 1197
corruptionPoints Angular = 25137

------------ Parsing

parse :: String -> [Symbol]
parse ""       = []
parse ('(':xs) = Open Round : parse xs
parse ('[':xs) = Open Square : parse xs
parse ('{':xs) = Open Curly : parse xs
parse ('<':xs) = Open Angular : parse xs
parse (')':xs) = Closed Round : parse xs
parse (']':xs) = Closed Square : parse xs
parse ('}':xs) = Closed Curly : parse xs
parse ('>':xs) = Closed Angular : parse xs
parse (_:xs)   = parse xs
