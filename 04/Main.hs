module Main where

import           Board
import           Control.Applicative (Alternative ((<|>)))
import           Data.Char           (isDigit)
import           Data.List           (partition)
import           Data.Maybe          (listToMaybe)
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.String

---------- Types

data Game = Game [Int] [Board]

data Winner = Winner Int Board

instance Eq Winner where
  Winner _ b1 == Winner _ b2 = b1 == b2

---------- Main logic

main :: IO ()
main = do
  Right game <- parse pFile "" <$> readFile "input.txt"
  putStr "Solution part 1: " >> print (winnerValue <$> playGame game)
  putStr "Solution part 2: " >> print (winnerValue <$> playGameSquid game)

playGame :: Game -> Maybe Winner
playGame = listToMaybe . winners

playGameSquid :: Game -> Maybe Winner
playGameSquid = listToMaybe . reverse . winners

winners :: Game -> [Winner]
winners = winners' []
  where
    winners' acc (Game (n:ns) boards) =
      let boards' = map (mark n) boards
          (wins, notWins) = partition isWinner boards'
      in winners' (acc ++ map (Winner n) wins) (Game ns notWins)
    winners' acc _ = acc

winnerValue :: Winner -> Int
winnerValue (Winner n board) = n * unmarkedSum board

---------- Parsers

pFile :: Parser Game
pFile = Game <$> (pNumbers <* newline <* newline) <*> many1 pBoard

pBoard :: Parser Board
pBoard = count 5 pRow
  where
    pRow = optional space *> count 5 (pBoardNumber <* many space) <* optional newline
    pBoardNumber = Unmarked <$> number

pNumbers :: Parser [Int]
pNumbers = number `sepBy` char ','

number :: Parser Int
number = read <$> many1 (satisfy isDigit)
