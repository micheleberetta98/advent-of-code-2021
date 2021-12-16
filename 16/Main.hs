module Main where

import           Data.Char          (digitToInt)
import           Data.List          (foldl')
import           Data.Maybe
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

calcValue :: Packet -> Maybe Int
calcValue (Literal _ v)     = Just v
calcValue (Operator _ 0 ps) = sum <$> mapM calcValue ps
calcValue (Operator _ 1 ps) = product <$> mapM calcValue ps
calcValue (Operator _ 2 ps) = minimum <$> mapM calcValue ps
calcValue (Operator _ 3 ps) = maximum <$> mapM calcValue ps
calcValue (Operator _ 5 ps) = applyFirstTwo (>) =<< mapM calcValue ps
calcValue (Operator _ 6 ps) = applyFirstTwo (<) =<< mapM calcValue ps
calcValue (Operator _ 7 ps) = applyFirstTwo (==) =<< mapM calcValue ps
calcValue _                 = Nothing

applyFirstTwo :: (Int -> Int -> Bool) -> [Int] -> Maybe Int
applyFirstTwo f (a:b:_) = Just $ if f a b then 1 else 0
applyFirstTwo _ _       = Nothing

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
bits n = binToInt <$> count n (char '0' <|> char '1')

------------ Utils

binToInt :: String -> Int
binToInt = foldl' (\acc x -> 2 * acc + x) 0 . map digitToInt

toBits :: String -> String
toBits = concat . mapMaybe toBin
  where
    toBin '0' = Just "0000"
    toBin '1' = Just "0001"
    toBin '2' = Just "0010"
    toBin '3' = Just "0011"
    toBin '4' = Just "0100"
    toBin '5' = Just "0101"
    toBin '6' = Just "0110"
    toBin '7' = Just "0111"
    toBin '8' = Just "1000"
    toBin '9' = Just "1001"
    toBin 'A' = Just "1010"
    toBin 'B' = Just "1011"
    toBin 'C' = Just "1100"
    toBin 'D' = Just "1101"
    toBin 'E' = Just "1110"
    toBin 'F' = Just "1111"
    toBin _   = Nothing
