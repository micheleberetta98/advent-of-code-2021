module Main where

import           Data.Char
import           Data.List
import           Text.Parsec
import           Text.Parsec.String

data Packet
  = Literal { version :: Int, value :: Int }
  | Operator { version :: Int, tag :: Int, content :: [Packet] }
  deriving (Show)

main :: IO ()
main = do
  p <- parse packet "" . toBits <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (versionSum <$> p)

------------ Solutions

versionSum :: Packet -> Int
versionSum (Literal v _)          = v
versionSum (Operator v _ content) = v + sum (map versionSum content)

------------ Parsing

packet :: Parser Packet
packet = do
  v   <- bits 3
  tag <- bits 3
  if tag == 4
    then Literal v      <$> literal
    else Operator v tag <$> operator

literal :: Parser Int
literal = do
  b <- bits 1
  bs <- bits 4
  if b == 1
    then (16 * bs +) <$> literal
    else pure bs

operator :: Parser [Packet]
operator = do
  lengthID <- bits 1
  if lengthID == 1
    then bits 11 >>= flip count packet
    else bits 15 >>= parseSpan
  where
    parseSpan 0 = pure []
    parseSpan n = do
      of0 <- getOffset
      p <- packet
      of1 <- getOffset
      (:) p <$> parseSpan (n - (of1 - of0))
    getOffset = sourceColumn <$> getPosition

bits :: Int -> Parser Int
bits n = foldl' (\acc x -> 2 * acc + x) 0 . map digitToInt <$> count n (char '0' <|> char '1')

toBits :: String -> String
toBits = concatMap (pad4 . toBin . digitToInt)
  where
    toBin 0 = "0"
    toBin x = toBin (x `div` 2) ++ show (x `mod` 2)
    pad4 xs = reverse . take 4 $ reverse xs ++ repeat '0'
