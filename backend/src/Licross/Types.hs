{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Licross.Types where

import Data.Function (on)
-- base
import qualified Data.List as L
import qualified Data.Maybe
import qualified Data.Ord
import GHC.Generics (Generic)

-- text
import qualified Data.Text as T
import Data.Text (Text(..))

-- unordered-containers
import qualified Data.HashMap.Strict as M

-- hashable
import Data.Hashable

-- lens
import Control.Lens

data Position = Position
  { yPos :: Integer
  , xPos :: Integer
  } deriving (Show, Eq, Generic, Ord)

instance Hashable Position

data TileType
  = Letter Text
  | Blank
  deriving (Show, Eq)

data Tile = Tile
  { tileText :: TileType
  , tilePoints :: Integer
  } deriving (Show, Eq)

data PlacedTile =
  PlacedTile Text
             Tile
  deriving (Show, Eq)

data Bonus
  = None
  | CompositeBonus Bonus
                   Bonus
  | WordMultiplier Integer
  | LetterMultiplier Integer
  | Anchor
  deriving (Show, Eq)

data Space = Space
  { spaceBonus :: Bonus
  , _spaceOccupant :: Maybe PlacedTile
  } deriving (Eq, Show)

spaceOccupant :: Lens' Space (Maybe PlacedTile)
spaceOccupant f parent =
  fmap (\x -> parent {_spaceOccupant = x}) (f (_spaceOccupant parent))

data Player = Player
  { playerRack :: [Tile]
  , playerScore :: Integer
  , playerName :: Text
  } deriving (Show)

newtype PlayerId =
  PlayerId Integer

data Move =
  PlayTiles PlayerId
            (M.HashMap Position PlacedTile)

type Board = M.HashMap Position Space

data Game = Game
  { _gameBoard :: Board
  , _gameBag :: [Tile]
  , _gamePlayers :: [Player]
  }

gameBoard :: Lens' Game Board
gameBoard f board = fmap (\x -> board {_gameBoard = x}) (f (_gameBoard board))

showSpace :: Space -> T.Text
showSpace space =
  case view spaceOccupant space of
    Nothing -> showBonus (spaceBonus space)
    Just (PlacedTile text _) -> " " <> text <> " "

showBonus :: Bonus -> T.Text
showBonus None = "   "
showBonus (WordMultiplier 2) = "DWS"
showBonus (WordMultiplier 3) = "TWS"
showBonus (LetterMultiplier 2) = "DLS"
showBonus (LetterMultiplier 3) = "TLS"
showBonus Anchor = " * "
showBonus (CompositeBonus a _) = showBonus a

showBoard :: Board -> T.Text
showBoard board =
  let positions =
        L.groupBy ((==) `on` (yPos . fst)) . L.sortBy (compare `on` fst) $
        M.toList board
   in T.intercalate "\n" .
      map ((<>) " " . T.intercalate " | " . map (showSpace . snd)) $
      positions

-- Standard instances
instance Semigroup Bonus where
  (<>) = CompositeBonus

instance Monoid Bonus where
  mempty = None
