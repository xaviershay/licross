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

applyMove :: Move -> Game -> Game
applyMove (PlayTiles pid tiles) = playTiles pid tiles

playTiles :: PlayerId -> M.HashMap Position PlacedTile -> Game -> Game
playTiles pid tiles game = foldl playTile game (M.toList tiles)
  where
    playTile :: Game -> (Position, PlacedTile) -> Game
    playTile game (pos, tile) =
      let Board board = gameBoard game
       in game
            { gameBoard =
                Board $
                M.adjust (\space -> space {spaceOccupant = Just tile}) pos board
            }

mkPlacedTile letter score =
  PlacedTile letter (Tile {tileText = Letter letter, tilePoints = score})

emptyGame =
  Game
    { gameBoard =
        Board $
        M.fromList
          [ ( Position {xPos = x, yPos = y}
            , Space (bonusFor x y) Nothing
                --(Just $ PlacedTile "A" (Tile {tileText = Letter "A", tilePoints = 1}))
             )
          | x <- [0 .. 8]
          , y <- [0 .. 8]
          ]
    , gameBag = mempty
    , gamePlayers = mempty
    }
