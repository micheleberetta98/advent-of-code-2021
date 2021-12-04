module Board
  ( Board
  , BoardNumber(..)
  , mark
  , isWinner
  , isMarked
  , isUnmarked
  , value
  )
where

import           Data.List


------------ Types

type Board = [[BoardNumber]]

data BoardNumber = Marked Int | Unmarked Int
  deriving (Eq, Show)

------------ Functions

mark :: Int -> Board -> Board
mark n (r:rs) = map (when (== Unmarked n) markNumber) r : mark n rs
  where
    markNumber (Unmarked x) = Marked x
    markNumber x            = x
mark _ b      = b

isWinner :: Board -> Bool
isWinner b = isRowFull b || isColFull b
  where
    isRowFull = any (all isMarked)
    isColFull = isRowFull . transpose

isMarked :: BoardNumber -> Bool
isMarked (Marked _) = True
isMarked _          = False

isUnmarked :: BoardNumber -> Bool
isUnmarked = not . isMarked

value :: BoardNumber -> Int
value (Marked x)   = x
value (Unmarked x) = x

------------ Local utils

when :: (a -> Bool) -> (a -> a) -> a -> a
when cond f x = if cond x then f x else x
