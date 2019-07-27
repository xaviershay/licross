{-# LANGUAGE OverloadedStrings #-}

-- Helper script to generate JSON templates.
module Main where

-- unordered-containers
import qualified Data.HashMap.Strict as M

import Licross.Prelude
import Licross.FakeData
import Licross.Builder

main :: IO ()
main =
  printAsJson
  . putTilesInBag 3 "" 0
  . putTilesInBag 10 "A" 1
  . putTilesInBag 2 "B" 3
  . putTilesInBag 2 "C" 4
  . putTilesInBag 4 "D" 2
  . putTilesInBag 11 "E" 1
  . putTilesInBag 2 "F" 4
  . putTilesInBag 3 "G" 2
  . putTilesInBag 2 "H" 4
  . putTilesInBag 8 "I" 1
  . putTilesInBag 1 "J" 8
  . putTilesInBag 1 "K" 5
  . putTilesInBag 4 "L" 1
  . putTilesInBag 2 "M" 3
  . putTilesInBag 6 "N" 1
  . putTilesInBag 8 "O" 1
  . putTilesInBag 2 "P" 3
  . putTilesInBag 1 "Q" 10
  . putTilesInBag 6 "R" 1
  . putTilesInBag 4 "S" 1
  . putTilesInBag 6 "T" 1
  . putTilesInBag 4 "U" 2
  . putTilesInBag 2 "V" 4
  . putTilesInBag 2 "W" 4
  . putTilesInBag 1 "X" 8
  . putTilesInBag 2 "Y" 4
  . putTilesInBag 1 "Z" 10
  $ titleGameBoard
