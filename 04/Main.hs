module Main where

import           Board
import           Data.Char
import           Data.List
import           Text.Parsec
import           Text.Parsec.String

---------- Main logic

data Game = Game [Int] [Board]
  deriving (Show)

main :: IO ()
main = do
  Right game <- parse pFile "" <$> readFile "input.txt"
  putStr "Solution part 1:    "
  case playGame game of
    Just (n, board) -> print (n * unmarkedSum board)
    Nothing         -> putStrLn "(no winners)"

playGame :: Game -> Maybe (Int, Board)
playGame (Game (n:ns) boards) =
  let boards' = map (mark n) boards in
  case find isWinner boards' of
    Just winner -> Just (n, winner)
    _           -> playGame $ Game ns boards'
playGame _        = Nothing

unmarkedSum :: Board -> Int
unmarkedSum = sum . concatMap (map value . filter isUnmarked)

---------- Parsers

pFile :: Parser Game
pFile =
  Game
  <$> pNumbers <* newline <* newline
  <*> many1 pBoard

pBoard :: Parser Board
pBoard = count 5 pRow
  where
    pRow = optional space *> count 5 (pBoardNumber <* many space) <* optional (char '\n')
    pBoardNumber = Unmarked <$> number

pNumbers :: Parser [Int]
pNumbers = number `sepBy` char ','

number :: Parser Int
number = read <$> many1 (satisfy isDigit)
