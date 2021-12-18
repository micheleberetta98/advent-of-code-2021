{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Either                (fromRight)
import           Data.List
import           Data.Maybe
import qualified Data.Set                   as Set
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char.Lexer (decimal)

data Pairs = Number Int | Pair Pairs Pairs deriving (Eq)
data Tok = Open | Close | Value Int deriving (Eq, Ord)

main :: IO ()
main = do
  Right ps <- traverse (parse pairsParser "") . lines <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (magnitude $ snailAdd ps)
  putStr "Answer 2:  " >> print (maximum . map (magnitude . snailAdd2) $ allPairs ps)

snailAdd :: [Pairs] -> Pairs
snailAdd = foldl1' snailAdd'
  where snailAdd' l r = reduce $ Pair l r

snailAdd2 :: (Pairs, Pairs) -> Pairs
snailAdd2 (a, b) = reduce $ Pair a b

allPairs :: [Pairs] -> [(Pairs, Pairs)]
allPairs ps = concat [[(x, y), (y, x)] | x <- ps, y <- ps, x /= y]

magnitude :: Pairs -> Int
magnitude (Number x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

reduce :: Pairs -> Pairs
reduce = fromJust . parseMaybe tokenParser . explode 0 [] . flatten

explode :: Int -> [Tok] -> [Tok] -> [Tok]
explode n prev (Open : Value x : Value y : Close : rest)
  | n >= 4                    = explode 0 [] $ reverse (addFirstValue x prev) ++ Value 0 : addFirstValue y rest
explode n prev (Open : rest)  = explode (n+1) (Open : prev) rest
explode n prev (Close : rest) = explode (n-1) (Close : prev) rest
explode n prev (x : rest)     = explode n (x : prev) rest
explode _ prev []             = split [] $ reverse prev

split :: [Tok] -> [Tok] -> [Tok]
split prev (Value x : rest)
  | x > 9             = explode 0 [] $ reverse prev ++ Open : Value (x `div` 2) : Value ((x + 1) `div` 2) : Close : rest
split prev (x : rest) = split (x : prev) rest
split prev []         = reverse prev

addFirstValue :: Int -> [Tok] -> [Tok]
addFirstValue k (Value x : rest) = Value (x + k) : rest
addFirstValue k (x : xs)         = x : addFirstValue k xs
addFirstValue _ []               = []

flatten :: Pairs -> [Tok]
flatten (Number x) = [Value x]
flatten (Pair l r) = [Open] ++ flatten l ++ flatten r ++ [Close]

pairsParser :: Parsec Void String Pairs
pairsParser = choice
  [ between (single '[') (single ']') $ Pair <$> (pairsParser <* single ',') <*> pairsParser
  , Number <$> decimal
  ]

tokenParser :: Parsec Void [Tok] Pairs
tokenParser = choice
  [ between (single Open) (single Close) $ Pair <$> tokenParser <*> tokenParser
  , Number <$> value
  ]
  where
    value = flip token Set.empty $ \case
      Value x -> Just x
      _       -> Nothing
