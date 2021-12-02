module Main where

import           Data.Char          (isDigit)
import           Data.List          (foldl')
import           Text.Parsec        (choice, many1, newline, parse, satisfy,
                                     sepEndBy, string)
import           Text.Parsec.String (Parser)

data Instruction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

data Position = Position { horizontal :: Int, depth :: Int } deriving (Show)

main :: IO ()
main = do
  lst <- parse pInstructionList "" <$> readFile "input.txt"
  case foldl' applyInstruction (Position 0 0) <$> lst of
    Right (Position a b) -> print (a * b)
    err                  -> print err

applyInstruction :: Position -> Instruction -> Position
applyInstruction (Position hor depth) (Forward x) = Position (hor + x) depth
applyInstruction (Position hor depth) (Down x)    = Position hor (depth + x)
applyInstruction (Position hor depth) (Up x)      = Position hor (depth - x)

pInstructionList :: Parser [Instruction]
pInstructionList = sepEndBy pInstruction newline

pInstruction :: Parser Instruction
pInstruction = choice
  [ string "forward " *> (Forward <$> number)
  , string "up "      *> (Up      <$> number)
  , string "down "    *> (Down    <$> number)
  ]
  where number = read <$> many1 (satisfy isDigit)
