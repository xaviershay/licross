module Licross.FakeData where

-- unordered-containers
import qualified Data.HashMap.Strict as M

import Licross.Prelude
import Licross.Types

bonusFor 4 4 = Anchor
bonusFor 0 0 = WordMultiplier 3
bonusFor 0 8 = WordMultiplier 3
bonusFor 8 0 = WordMultiplier 3
bonusFor 8 8 = WordMultiplier 3
bonusFor _ _ = None

applyMove :: Move -> Game -> Game
applyMove (PlayTiles pid tiles) = playTiles pid tiles

playTiles :: PlayerId -> M.HashMap Position PlacedTile -> Game -> Game
playTiles pid tiles game = foldl playTile game (M.toList tiles)
  where
    playTile game (pos, tile) =
      set (gameBoard . at pos . _Just . spaceOccupant) (Just tile) game

mkPlacedTile letter score =
  PlacedTile letter (Tile {tileText = Letter letter, tilePoints = score})

emptyGame =
  Game
    { _gameBoard =
        M.fromList
          [ ( Position {xPos = x, yPos = y}
            , Space (bonusFor x y) Nothing
                --(Just $ PlacedTile "A" (Tile {tileText = Letter "A", tilePoints = 1}))
             )
          | x <- [0 .. 8]
          , y <- [0 .. 8]
          ]
    , _gameBag = mempty
    , _gamePlayers = mempty
    }
