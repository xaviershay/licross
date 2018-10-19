{-# LANGUAGE OverloadedStrings #-}

module Licross.FakeData where

-- unordered-containers
import qualified Data.HashMap.Strict as M

import Licross.Types

bonusFor 4 4 = Anchor
bonusFor 0 0 = WordMultiplier 3
bonusFor 0 8 = WordMultiplier 3
bonusFor 8 0 = WordMultiplier 3
bonusFor 8 8 = WordMultiplier 3
bonusFor _ _ = None

emptyGame =
  Game
    { gameBoard =
        Board $
        M.fromList
          [ ( Position {xPos = x, yPos = y}
            , Space
                (bonusFor x y)
                Nothing
                --(Just $ PlacedTile "A" (Tile {tileText = Letter "A", tilePoints = 1}))
                )
          | x <- [0 .. 8]
          , y <- [0 .. 8]
          ]
    , gameBag = mempty
    , gamePlayers = mempty
    }
