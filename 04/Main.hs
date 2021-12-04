{-# LANGUAGE TupleSections #-}

module Main where

import           Board
import           Control.Applicative (Alternative ((<|>)))
import           Data.Char           (isDigit)
import           Data.List           ((\\))
import           Data.Maybe          (listToMaybe)
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.String

---------- Types

data Game = Game [Int] [Board]
  deriving (Show)

data Winner = Winner Int Board
  deriving (Show)

instance Eq Winner where
  Winner _ b1 == Winner _ b2 = b1 == b2

---------- Main logic

main :: IO ()
main = do
  Right game <- parse pFile "" <$> readFile "input.txt"
  putStr "Solution part 1:    "
  case playGame game of
    Just (Winner n board) -> print (n * unmarkedSum board)
    Nothing               -> putStrLn "(no winners)"

  putStr "Solution part 2:    "
  case playGameSquid game of
    Just (Winner n board) -> print (n * unmarkedSum board)
    Nothing               -> putStrLn "(no winners)"

playGame :: Game -> Maybe Winner
playGame = listToMaybe . winners

playGameSquid :: Game -> Maybe Winner
playGameSquid = listToMaybe . reverse . winners

winners :: Game -> [Winner]
winners = winners' []
  where
    winners' acc (Game (n:ns) boards) =
      let boards' = map (mark n) boards
          ws = map (Winner n) $ filter isWinner boards'
      in winners' (acc ++ (ws \\ acc)) (Game ns boards')
    winners' acc _ = acc

---------- Parsers

pFile :: Parser Game
pFile = Game <$> (pNumbers <* newline <* newline) <*> many1 pBoard

pBoard :: Parser Board
pBoard = Board <$> count 5 pRow
  where
    pRow = optional space *> count 5 (pBoardNumber <* many space) <* optional newline
    pBoardNumber = Unmarked <$> number

pNumbers :: Parser [Int]
pNumbers = number `sepBy` char ','

number :: Parser Int
number = read <$> many1 (satisfy isDigit)
