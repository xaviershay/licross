{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
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
  , GameId
  , Tile(..)
  , TileType(..)
  , Player(..)
  -- Constructors
  , newGameId
  , gameIdFromText
  , gameIdToText
  , mkPos
  , mkPlacedTile
  , mkTile
  , emptySpace
  , emptyGame
  -- lens
  , spaceBonus
  , spaceOccupant
  , gameBoard
  , gamePlayers
  , gameBag
  , gameVersion
  , showBoard
  -- accessors
  , xPos
  , yPos
  , tileScore
  , tileLetter
  ) where

-- aeson
import           Data.Aeson           ((.=))
import qualified Data.Aeson

-- base
import           GHC.Generics         (Generic)

-- text
import           Data.Text            (Text (..))
import qualified Data.Text            as T

-- unordered-containers
import qualified Data.HashMap.Strict  as M

-- hashable
import           Data.Hashable        (Hashable (..))

-- lens
import qualified Control.Lens

-- licross
import           Licross.Extras.Aeson
import           Licross.Prelude

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

instance Data.Aeson.ToJSON TileType where
  toJSON Blank = Data.Aeson.toJSON ("" :: Data.Text.Text)
  toJSON (Letter x) = Data.Aeson.toJSON x

data Tile = Tile
  { _tileLetter :: TileType
  , _tileScore :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving Data.Aeson.ToJSON via StripPrefix "_tile" Tile

mkTile :: T.Text -> Int -> Tile
mkTile "" score = Tile { _tileLetter = Blank, _tileScore = score }
mkTile letter score = Tile { _tileLetter = Letter letter, _tileScore = score }

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
  { _playerRack :: [Tile]
  , _playerScore :: Int
  , _playerName :: Text
  }
  deriving stock (Show, Generic)
  deriving Data.Aeson.ToJSON via StripPrefix "_player" Player

newtype PlayerId =
  PlayerId Integer
  deriving (Show, Eq, Generic)

-- TODO: Need to move PlayerId outside this type, so that API FromJSON etc can
-- work nicely.
data Move =
  PlayTiles PlayerId
            (M.HashMap Position PlacedTile)

type Board = M.HashMap Position Space

data Game = Game
  { _gameBoard :: Board
  , _gameBag :: [Tile]
  , _gamePlayers :: [Player]
  , _gameVersion :: Int
  } deriving (Show)

-- A redacted type if effectively a newtype that allows for different JSON
-- representations of an object to be shown to different players/observers.
data RedactedGame =
  RedactedGame (Maybe PlayerId)
               Game

newtype GameId =
  GameId T.Text
  deriving (Show, Eq, Generic)

instance Hashable GameId

newGameId :: IO GameId
newGameId = GameId <$> randomId 25

gameIdFromText :: T.Text -> Maybe GameId
gameIdFromText = pure . GameId

gameIdToText :: GameId -> T.Text
gameIdToText (GameId x) = x

gameBoard :: Control.Lens.Lens' Game Board
gameBoard f board = fmap (\x -> board {_gameBoard = x}) (f (_gameBoard board))

gameBag :: Control.Lens.Lens' Game [Tile]
gameBag f parent = fmap (\x -> parent {_gameBag = x}) (f (_gameBag parent))

gamePlayers :: Control.Lens.Lens' Game [Player]
gamePlayers f parent =
  fmap (\x -> parent {_gamePlayers = x}) (f (_gamePlayers parent))

gameVersion :: Control.Lens.Lens' Game Int
gameVersion f parent =
  fmap (\x -> parent {_gameVersion = x}) (f (_gameVersion parent))

tileScore :: Control.Lens.Lens' Tile Int
tileScore f parent =
  fmap (\x -> parent {_tileScore = x}) (f (_tileScore parent))

tileLetter :: Control.Lens.Lens' Tile TileType
tileLetter f parent =
  fmap (\x -> parent {_tileLetter = x}) (f (_tileLetter parent))

playerRack :: Control.Lens.Lens' Player [Tile]
playerRack f parent =
  fmap (\x -> parent {_playerRack = x}) (f (_playerRack parent))

playerScore :: Control.Lens.Lens' Player Int
playerScore f parent =
  fmap (\x -> parent {_playerScore = x}) (f (_playerScore parent))

playerName :: Control.Lens.Lens' Player Text
playerName f parent =
  fmap (\x -> parent {_playerName = x}) (f (_playerName parent))

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

instance Semigroup Position where
  a <> b = mkPos (xPos a + xPos b) (yPos a + yPos b)

instance Monoid Position where
  mempty = mkPos 0 0

-- Constructors
-- TODO: This isn't actually an empty constructor, board should be empty too.
emptyGame =
  Game
    { _gameBoard =
        M.fromList
          [(mkPos x y, Space None Nothing) | x <- [0 .. 14], y <- [0 .. 14]]
    , _gameBag = mempty
    , _gamePlayers = mempty
    , _gameVersion = 0
    }

mkPlacedTile letter score =
  PlacedTile letter (Tile {_tileLetter = Letter letter, _tileScore = score})
