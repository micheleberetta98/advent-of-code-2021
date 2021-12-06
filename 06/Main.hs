{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Text as T

type LanternfishCount = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

main :: IO ()
main = do
  fish <- parse <$> readFile "input.txt"
  putStr "Answer 1:   " >> print (fishAtDay 80 fish)
  putStr "Answer 2:   " >> print (fishAtDay 256 fish)

------------ Functions

fishAtDay :: Int -> LanternfishCount -> Int
fishAtDay !n = count . apply n step
  where
    count (a, b, c, d, e, f, g, h, i) = a + b + c + d + e + f + g + h + i
    step (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h + a, i, a)

apply :: Int -> (a -> a) -> a -> a
apply 0 f !x  = x
apply !n f !x = apply (n-1) f (f x)

------------ Parsing

parse :: String -> LanternfishCount
parse = format . freqs . map read . splitOn ","
  where
    freqs :: [Int] -> [Int]
    freqs l = map (\x -> length $ filter (== x) l) [1 .. 5]
    format [a, b, c, d, e] = (0, a, b, c, d, e, 0, 0, 0) -- In the file there are only values from 1 to 5

splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)
