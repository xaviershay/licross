module Licross.FakeData where

import qualified Data.HashMap.Strict as M -- unordered-containers
import qualified Data.Text as T -- text

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

setBonus :: Bonus -> Position -> Game -> Game
setBonus bonus pos = set (gameBoard . at pos . _Just . spaceBonus) bonus

-- A fake game board showing some welcome text.
titleGame :: Game
titleGame =
  applyMove -- TODO: Quick hack to get tiles on board, won't work with validation.
    (PlayTiles
       (PlayerId 1)
       (mkWord (root <> mkPos 1 0) "WELCOME" vertical <>
        mkWord (root <> mkPos 0 4) "TO" horizontal <>
        mkWord (root <> mkPos 1 2) "LICROSS" horizontal <>
        mkWord (root <> mkPos 5 1) "WORD" vertical <>
        mkWord (root <> mkPos 7 (-2)) "GAMES" vertical)) .
  foldl (\game (bonus, pos) -> setBonus bonus pos game) emptyGame $
  bonuses
  where
    bonuses =
      concat
        [ f (WordMultiplier 3) 0 3
        , f (WordMultiplier 2) 5 2
        , f (WordMultiplier 2) 7 4
        , f (LetterMultiplier 3) 1 1
        , f (LetterMultiplier 2) 2 2
        , f (LetterMultiplier 2) 3 3
        , f (LetterMultiplier 3) 4 4
        , f (LetterMultiplier 2) 5 5
        , f (LetterMultiplier 2) 6 6
        , f (LetterMultiplier 3) 7 0
        , f (Anchor) 7 7
        ]
      where
        f bonus x y = [(bonus, uncurry mkPos p) | p <- reflect x y]
    -- Don't bother filtering out duplicates
    reflect x y =
      [ (x, y)
      , (x, 14 - y)
      , (14 - x, y)
      , (14 - x, 14 - y)
      , (y, x)
      , (y, 14 - x)
      , (14 - y, x)
      , (14 - y, 14 - x)
      ]
    root = mkPos 3 5
    scores =
      M.fromList $
      [ ("A", 1)
      , ("C", 4)
      , ("D", 2)
      , ("E", 1)
      , ("G", 2)
      , ("I", 1)
      , ("L", 1)
      , ("M", 3)
      , ("O", 1)
      , ("R", 1)
      , ("S", 1)
      , ("T", 1)
      , ("W", 4)
      ]
    horizontal i = mkPos i 0
    vertical i = mkPos 0 i
    mkWord pos word direction =
      (M.fromList $
       map
         (\(letter, i) ->
            ( pos <> direction i
            , mkPlacedTile (T.pack [letter]) (M.lookupDefault 0 [letter] scores)))
         (zip word [0 ..]))
