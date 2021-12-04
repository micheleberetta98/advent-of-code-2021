module Board
  ( Board(..)
  , BoardNumber(..)
  , mark
  , isWinner
  , unmarkedSum
  )
where

import           Data.List (transpose)

------------ Types

newtype Board = Board [[BoardNumber]]
  deriving (Show)

data BoardNumber = Marked Int | Unmarked Int
  deriving (Eq, Show)

instance Eq Board where
  Board b1 == Board b2 = map (map value) b1 == map (map value) b2

------------ Functions

mark :: Int -> Board -> Board
mark n (Board (r:rs)) =
  let Board rest = mark n (Board rs)
  in Board $ map (when (== Unmarked n) markNumber) r : rest
mark _ b              = b

isWinner :: Board -> Bool
isWinner (Board b) = isRowFull b || isColFull b
  where
    isRowFull = any (all isMarked)
    isColFull = isRowFull . transpose

unmarkedSum :: Board -> Int
unmarkedSum (Board b) = sum . concatMap (map value . filter isUnmarked) $ b

------------ Local utils

when :: (a -> Bool) -> (a -> a) -> a -> a
when cond f x = if cond x then f x else x

isMarked :: BoardNumber -> Bool
isMarked (Marked _) = True
isMarked _          = False

isUnmarked :: BoardNumber -> Bool
isUnmarked = not . isMarked

markNumber :: BoardNumber -> BoardNumber
markNumber (Unmarked x) = Marked x
markNumber x            = x

value :: BoardNumber -> Int
value (Marked x)   = x
value (Unmarked x) = x
