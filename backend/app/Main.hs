{-# LANGUAGE OverloadedStrings #-}

module Main where

-- unordered-containers
import qualified Data.HashMap.Strict as M

import Licross.FakeData
import Licross.Types

import qualified Data.Text as T

main :: IO ()
main =
  putStrLn .
  T.unpack .
  showBoard .
  gameBoard .
  applyMove
    (PlayTiles
       (PlayerId 1)
       (M.fromList
          [ (Position 1 1, mkPlacedTile "A" 1)
          , (Position 1 2, mkPlacedTile "B" 3)
          ])) $
  emptyGame

focus = main
