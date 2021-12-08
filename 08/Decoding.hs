module Decoding where

import           Control.Monad
import           Data.List
import           Data.Maybe

------------ Utility types

type Wire = Char
type Wires = [Wire]
type WireSubstitution = [(Wire, Wire)]

------------ Functions

decodeWith :: WireSubstitution -> [Wires] -> Int
decodeWith solution = toDec . map (wiresValue . substituteWith solution)
  where toDec = foldl' (\acc x -> 10 * acc + x) 0

findSolution :: [Wires] -> WireSubstitution
findSolution = head . foldl' valid allPossibleAssocs
  where
    valid assocs ws = filter (isValid ws) assocs
    allPossibleAssocs = map (`zip` wires) (permutations wires)
    wires = ['a'..'g']

------------ Local utilities

isValid :: Wires -> WireSubstitution -> Bool
isValid xs assoc = (`elem` constraints) . sort $ substituteWith assoc xs

substituteWith :: WireSubstitution -> Wires -> Wires
substituteWith assoc = sort . map substituteWire
  where substituteWire x = fromJust (lookup x assoc)

wiresValue :: Wires -> Int
wiresValue x = fromJust (elemIndex x constraints)

constraints :: [Wires]
constraints = [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9]
  where
    _0 = "abcefg"
    _1 = "cf"
    _2 = "acdeg"
    _3 = "acdfg"
    _4 = "bcdf"
    _5 = "abdfg"
    _6 = "abdefg"
    _7 = "acf"
    _8 = "abcdefg"
    _9 = "abcdfg"
