module Board
  ( Board(..)
  , BoardNumber(..)
  , mark
  , isWinner
  , unmarkedSum
  )
where

import           Data.Function (on)
import           Data.List     (transpose)

------------ Types

type Board = [[BoardNumber]]

data BoardNumber =
  Marked { value :: Int }
  | Unmarked { value :: Int }

instance Eq BoardNumber where
  (==) = (==) `on` value

------------ Functions

mark :: Int -> Board -> Board
mark n (r:rs) = map (when (hasValue n) markNumber) r : mark n rs
mark _ b      = b

isWinner :: Board -> Bool
isWinner b = isRowFull b || isColFull b
  where
    isRowFull = any (all isMarked)
    isColFull = isRowFull . transpose

unmarkedSum :: Board -> Int
unmarkedSum = sum . concatMap (map value . filter (not . isMarked))

------------ Local utils

when :: (a -> Bool) -> (a -> a) -> a -> a
when cond f x = if cond x then f x else x

isMarked :: BoardNumber -> Bool
isMarked (Marked _) = True
isMarked _          = False

markNumber :: BoardNumber -> BoardNumber
markNumber = Marked . value

hasValue :: Int -> BoardNumber -> Bool
hasValue n = (== n) . value
