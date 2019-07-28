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
  . fillBag
    [ mkTileSpec 3 "" 0
    , mkTileSpec 10 "A" 1
    , mkTileSpec 2 "B" 3
    , mkTileSpec 2 "C" 4
    , mkTileSpec 4 "D" 2
    , mkTileSpec 11 "E" 1
    , mkTileSpec 2 "F" 4
    , mkTileSpec 3 "G" 2
    , mkTileSpec 2 "H" 4
    , mkTileSpec 8 "I" 1
    , mkTileSpec 1 "J" 8
    , mkTileSpec 1 "K" 5
    , mkTileSpec 4 "L" 1
    , mkTileSpec 2 "M" 3
    , mkTileSpec 6 "N" 1
    , mkTileSpec 8 "O" 1
    , mkTileSpec 2 "P" 3
    , mkTileSpec 1 "Q" 10
    , mkTileSpec 6 "R" 1
    , mkTileSpec 4 "S" 1
    , mkTileSpec 6 "T" 1
    , mkTileSpec 4 "U" 2
    , mkTileSpec 2 "V" 4
    , mkTileSpec 2 "W" 4
    , mkTileSpec 1 "X" 8
    , mkTileSpec 2 "Y" 4
    , mkTileSpec 1 "Z" 10
    ]
  $ titleGameBoard
