module Main where

import           Data.Char          (digitToInt)
import           Data.List          (foldl')
import           Text.Parsec
import           Text.Parsec.String (Parser)

data Packet
  = Literal { version :: Int, value :: Int }
  | Operator { version :: Int, tag :: Int, content :: [Packet] }
  deriving (Show)

main :: IO ()
main = do
  Right p <- parse packet "" . toBits <$> readFile "input.txt"
  putStr "Answer 1:  " >> print (sumVersions p)
  putStr "Answer 2:  " >> print (calcValue p)


------------ Solutions

sumVersions :: Packet -> Int
sumVersions (Literal v _)          = v
sumVersions (Operator v _ content) = v + sum (sumVersions <$> content)

calcValue :: Packet -> Int
calcValue (Literal _ v)          = v
calcValue (Operator _ 0 ps)      = sum       (calcValue <$> ps)
calcValue (Operator _ 1 ps)      = product   (calcValue <$> ps)
calcValue (Operator _ 2 ps)      = minimum   (calcValue <$> ps)
calcValue (Operator _ 3 ps)      = maximum   (calcValue <$> ps)
calcValue (Operator _ 5 (a:b:_)) = boolToInt (calcValue a > calcValue b)
calcValue (Operator _ 6 (a:b:_)) = boolToInt (calcValue a < calcValue b)
calcValue (Operator _ 7 (a:b:_)) = boolToInt (calcValue a == calcValue b)
calcValue _                      = undefined

------------ Parsing

packet :: Parser Packet
packet = do
  ver <- bits 3
  tag <- bits 3
  if tag == 4
    then Literal  ver     <$> literal
    else Operator ver tag <$> operator

literal :: Parser Int
literal = literal' 0
  where
    literal' n = do
      b  <- bits 1
      n' <- (16 * n +) <$> bits 4
      (if b == 1 then literal' else pure) n'

operator :: Parser [Packet]
operator = do
  lengthID <- bits 1
  if lengthID == 1
    then bits 11 >>= flip count packet
    else bits 15 >>= span
  where
    span 0 = pure []
    span n = do
      of0 <- getOffset
      p   <- packet
      of1 <- getOffset
      (:) p <$> span (n - (of1 - of0))
    getOffset = sourceColumn <$> getPosition

bits :: Int -> Parser Int
bits n = foldl' (\acc x -> 2 * acc + x) 0 . map digitToInt <$> count n (char '0' <|> char '1')

------------ Utils

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

toBits :: String -> String
toBits = concatMap (pad4 . toBin . digitToInt)
  where
    toBin 0 = "0"
    toBin x = toBin (x `div` 2) ++ show (x `mod` 2)
    pad4 xs = reverse . take 4 $ reverse xs ++ repeat '0'
