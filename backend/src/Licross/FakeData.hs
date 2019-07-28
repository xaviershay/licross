module Licross.FakeData (titleGame, titleGameBoard) where

-- unordered-containers
import qualified Data.HashMap.Strict as M

-- text
import qualified Data.Text           as T

-- licross
import           Licross.Builder
import           Licross.Prelude
import           Licross.Types

--applyMove :: Move -> Game -> Game
--applyMove (PlayTiles pid tiles) = playTiles pid tiles

playTiles :: PlayerId -> M.HashMap Position PlacedTile -> Game -> Game
playTiles pid tiles game = foldl playTile game (M.toList tiles)
  where
    playTile game (pos, tile) =
      set (gameBoard . at pos . _Just . spaceOccupant) (Just tile) game

titleGameBoard :: Game
titleGameBoard = 
    setBonusWithSymmetry (WordMultiplier 3) 0 3
  . setBonusWithSymmetry (WordMultiplier 2) 5 2
  . setBonusWithSymmetry (WordMultiplier 2) 7 4
  . setBonusWithSymmetry (LetterMultiplier 3) 1 1
  . setBonusWithSymmetry (LetterMultiplier 2) 2 2
  . setBonusWithSymmetry (LetterMultiplier 2) 3 3
  . setBonusWithSymmetry (LetterMultiplier 3) 4 4
  . setBonusWithSymmetry (LetterMultiplier 2) 5 5
  . setBonusWithSymmetry (LetterMultiplier 2) 6 6
  . setBonusWithSymmetry (LetterMultiplier 3) 7 0
  . setBonusWithSymmetry (Anchor) 7 7
  $ emptyGame

-- A fake game board showing some welcome text.
titleGame :: Game
titleGame = emptyGame

-- TODO: Changing PlayTiles API and this is getting in the way. Bring it back later.
--  applyMove -- TODO: Quick hack to get tiles on board, won't work with validation.
--    (PlayTiles
--       (mkWord (root <> mkPos 1 0) "WELCOME" vertical <>
--        mkWord (root <> mkPos 0 4) "TO" horizontal <>
--        mkWord (root <> mkPos 1 2) "LICROSS" horizontal <>
--        mkWord (root <> mkPos 5 1) "WORD" vertical <>
--        mkWord (root <> mkPos 7 (-2)) "GAMES" vertical))
--    titleGameBoard
--  where
--    root = mkPos 3 5
--    scores =
--      M.fromList $
--      [ ("A", 1)
--      , ("C", 4)
--      , ("D", 2)
--      , ("E", 1)
--      , ("G", 2)
--      , ("I", 1)
--      , ("L", 1)
--      , ("M", 3)
--      , ("O", 1)
--      , ("R", 1)
--      , ("S", 1)
--      , ("T", 1)
--      , ("W", 4)
--      ]
--    horizontal i = mkPos i 0
--    vertical i = mkPos 0 i
--    mkWord pos word direction =
--      (
--       map
--         (\(letter, i) ->
--            ( pos <> direction i
--            , mkPlacedTile (T.pack [letter]) (M.lookupDefault 0 [letter] scores)))
--         (zip word [0 ..]))
