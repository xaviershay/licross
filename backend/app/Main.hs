{-# LANGUAGE OverloadedStrings #-}

module Main where

-- unordered-containers
import qualified Data.HashMap.Strict as M

import Licross.Prelude
import Licross.FakeData
import Licross.Types

import qualified Data.Text as T

main :: IO ()
main =
  putStrLn .
  T.unpack .
  showBoard .
  view gameBoard .
  applyMove
    (PlayTiles
       (PlayerId 1)
       (M.fromList
          [ (mkPos 1 1, mkPlacedTile "A" 1)
          , (mkPos 1 2, mkPlacedTile "B" 3)
          ])) $
  emptyGame

focus = main
