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

data Position = Position
  { horizontal :: Int
  , depth      :: Int
  , aim        :: Int
  } deriving (Show)

main :: IO ()
main = do
  lst <- parse pInstructionList "" <$> readFile "input.txt"
  case foldl' applyInstruction (Position 0 0 0) <$> lst of
    Right (Position a b _) -> print (a * b)
    err                    -> print err

applyInstruction :: Position -> Instruction -> Position
applyInstruction (Position hor depth aim) (Forward x) = Position (hor + x) (depth + aim * x) aim
applyInstruction (Position hor depth aim) (Down x)    = Position hor depth (aim + x)
applyInstruction (Position hor depth aim) (Up x)      = Position hor depth (aim - x)

pInstructionList :: Parser [Instruction]
pInstructionList = sepEndBy pInstruction newline

pInstruction :: Parser Instruction
pInstruction = choice
  [ string "forward " *> (Forward <$> number)
  , string "up "      *> (Up      <$> number)
  , string "down "    *> (Down    <$> number)
  ]
  where number = read <$> many1 (satisfy isDigit)
