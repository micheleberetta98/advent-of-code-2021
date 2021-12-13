module Main where

import           Data.Bifunctor (bimap)
import           Data.Char      (isDigit)
import           Data.List      (foldl')
import           Data.Set       (fromList)
import           Dot

main :: IO ()
main = do
  (dots, folds) <- parse <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (afterFirstFold folds dots)
  writeFile "output.txt" $ dotsMatrix (foldAll dots folds)
  putStrLn "Answer 2:  in output.txt"

------------ Solutions

afterFirstFold :: [Fold] -> Dots -> Int
afterFirstFold folds = length . foldAlong (head folds)

foldAll :: Dots -> [Fold] -> Dots
foldAll = foldl' (flip foldAlong)

------------ Parser

parse :: String -> (Dots, [Fold])
parse = format . fmap tail . break (== "") . lines
  where format (dots, folds) = (fromList (map readDot dots), map readFold folds)

readDot :: String -> (Int, Int)
readDot = bimap read read . splitOn ','

readFold :: String -> Fold
readFold = readFold' . splitOn '='
  where
    readFold' ("fold along x", n) = XFold (read n)
    readFold' ("fold along y", n) = YFold (read n)

splitOn :: Char -> String -> (String, String)
splitOn c = fmap tail . break (== c)
