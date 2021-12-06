module Lanternfish where

newtype Lanternfish = Lanternfish { value :: Int }

new :: Lanternfish
new = Lanternfish 8

reset :: Lanternfish -> Lanternfish
reset (Lanternfish 0) = Lanternfish 6
reset x               = x

spawn :: Lanternfish -> [Lanternfish]
spawn l@(Lanternfish 0) = [reset l, new]
spawn (Lanternfish x)   = [Lanternfish (x - 1)]
