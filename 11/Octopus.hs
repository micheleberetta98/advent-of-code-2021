module Octopus where

data Octopus
  = FlashedOctopus
  | Octopus Int
  deriving (Eq)

flashed :: Octopus -> Bool
flashed = (== FlashedOctopus)

increaseLuminosity :: Octopus -> Octopus
increaseLuminosity FlashedOctopus = FlashedOctopus
increaseLuminosity (Octopus 9)    = FlashedOctopus
increaseLuminosity (Octopus x)    = Octopus (x + 1)

resetOctopus :: Octopus -> Octopus
resetOctopus FlashedOctopus = Octopus 0
resetOctopus o              = o
