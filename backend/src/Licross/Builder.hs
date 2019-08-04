module Licross.Builder where

-- text
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- aeson
import qualified Data.Aeson
import qualified Data.Aeson.Text

-- unordered-container
import qualified Data.HashMap.Strict as M

-- licross
import Licross.Prelude
import Licross.Types
import Licross.Json

type TileSpec = (Int, T.Text, Int)

mkTileSpec a b c = (a, b, c)

fillBag :: [TileSpec] -> Game -> Game
fillBag spec =
  let expanded = concatMap expandSpec spec in
  let tiles = map buildTile $ zip expanded [1..] in

  set gameTiles (M.fromList $ map (\x -> (view tileId x, x)) tiles)

  where
    expandSpec (n, letter, score) = replicate n (letter, score)
    buildTile ((letter, score), id) = mkTile id letter score

setBonusWithSymmetry :: Bonus -> Integer -> Integer -> Game -> Game
setBonusWithSymmetry bonus x y game =
  let bonuses = [(bonus, uncurry mkPos p) | p <- reflect 14 x y] in

  foldl
    (\game (bonus, pos) ->
      set
        (gameBoard . at pos . _Just . spaceBonus)
        bonus
        game
    )
    game
    bonuses

printAsJson :: Game -> IO ()
printAsJson = putStrLn
  . LT.unpack
  . Data.Aeson.Text.encodeToLazyText
  . RedactedGame Nothing

-- 8-way symmetrical reflection of coordinates for a given grid size.
-- Doesn't bother to filter out duplicates, not needed.
reflect gridSize x y =
  [ (x, y)
  , (x, gridSize - y)
  , (gridSize - x, y)
  , (gridSize - x, gridSize - y)
  , (y, x)
  , (y, gridSize - x)
  , (gridSize - y, x)
  , (gridSize - y, gridSize - x)
  ]
