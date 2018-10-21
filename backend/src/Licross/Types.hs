{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Licross.Types
  -- Types
  ( Position
  , Game
  , Board(..)
  , PlacedTile(..)
  , PlayerId(..)
  , Move(..)
  , Bonus(..)
  , RedactedGame(..)
  , Space
  -- Constructors
  , mkPos
  , mkPlacedTile
  , emptySpace
  , emptyGame
  -- lens
  , spaceBonus
  , spaceOccupant
  , gameBoard
  , gamePlayers
  , gameBag
  , showBoard
  -- accessors
  , xPos
  , yPos
  , tilePoints
  ) where

-- base
import GHC.Generics (Generic)

-- text
import qualified Data.Text as T
import Data.Text (Text(..))

-- unordered-containers
import qualified Data.HashMap.Strict as M

-- hashable
import Data.Hashable (Hashable(..))

-- lens
import qualified Control.Lens

-- licross
import Licross.Prelude

data Position = Position
  { yPos :: Integer
  , xPos :: Integer
  } deriving (Show, Eq, Generic, Ord)

mkPos x y = Position {xPos = x, yPos = y}

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
  { _spaceBonus :: Bonus
  , _spaceOccupant :: Maybe PlacedTile
  } deriving (Eq, Show)

emptySpace = Space None Nothing

spaceOccupant :: Control.Lens.Lens' Space (Maybe PlacedTile)
spaceOccupant f parent =
  fmap (\x -> parent {_spaceOccupant = x}) (f (_spaceOccupant parent))

spaceBonus :: Control.Lens.Lens' Space Bonus
spaceBonus f parent =
  fmap (\x -> parent {_spaceBonus = x}) (f (_spaceBonus parent))

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

-- A redacted type if effectively a newtype that allows for different JSON
-- representations of an object to be shown to different players/observers.
data RedactedGame = RedactedGame (Maybe PlayerId) Game

newtype GameId = GameId Int deriving (Show, Eq)

gameBoard :: Control.Lens.Lens' Game Board
gameBoard f board = fmap (\x -> board {_gameBoard = x}) (f (_gameBoard board))

gameBag :: Control.Lens.Lens' Game [Tile]
gameBag f parent =
  fmap (\x -> parent {_gameBag = x}) (f (_gameBag parent))

gamePlayers :: Control.Lens.Lens' Game [Player]
gamePlayers f parent =
  fmap (\x -> parent {_gamePlayers = x}) (f (_gamePlayers parent))

showSpace :: Space -> T.Text
showSpace space =
  case view spaceOccupant space of
    Nothing -> showBonus (view spaceBonus space)
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
        groupBy ((==) `on` (yPos . fst)) . sortBy (compare `on` fst) $
        M.toList board
   in T.intercalate "\n" .
      map ((<>) " " . T.intercalate " | " . map (showSpace . snd)) $
      positions

-- Standard instances
instance Semigroup Bonus where
  (<>) = CompositeBonus

instance Monoid Bonus where
  mempty = None

-- Constructors
emptyGame =
  Game
    { _gameBoard =
        M.fromList
          [(mkPos x y, Space None Nothing) | x <- [0 .. 8], y <- [0 .. 8]]
    , _gameBag = mempty
    , _gamePlayers = mempty
    }

mkPlacedTile letter score =
  PlacedTile letter (Tile {tileText = Letter letter, tilePoints = score})
