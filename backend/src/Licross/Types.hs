{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Licross.Types
  -- Types
  ( Position
  , Game
  , Board(..)
  , PlayerId(..)
  , Move(..)
  , Bonus(..)
  , RedactedGame(..)
  , Space
  , GameId(..)
  , Tile(..)
  , TileId(..)
  , TileType(..)
  , TileLocation(..)
  , Player(..)
  -- Constructors
  , newGameId
  , gameIdFromText
  , gameIdToText
  , mkPos
  , mkPlayer
  , mkTile
  , emptySpace
  , emptyGame
  -- lens
  , playerId
  --, playerRack
  , spaceBonus
  , spaceOccupant
  , gameBoard
  , gamePlayers
  --, gameBag
  , gameVersion
  , gameTiles
  , showBoard
  -- accessors
  , xPos
  , yPos
  , tileId
  , tileScore
  , tileLetter
  , tileLocation
  ) where

-- aeson
import           Data.Aeson
import           Data.Aeson.Types (toJSONKeyText, Parser)

-- base
import           GHC.Generics         (Generic)
import           GHC.Int (Int32(..))
import           Text.Read (readMaybe)
import           Data.Foldable (toList)

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

instance ToJSON Position where
  toJSON (Position { xPos = x, yPos = y}) = toJSON [x, y]

instance ToJSONKey Position where
  toJSONKey = toJSONKeyText showPosition
    where
      showPosition (Position { xPos = x, yPos = y}) =
        T.pack (show x) <> "-" <> T.pack (show y)

instance FromJSON Position where
  parseJSON = withArray "Position" $ \v ->
    case toList v of
      [x, y] -> mkPos <$> parseJSON x <*> parseJSON y
      _      -> fail "Invalid Position"

instance FromJSONKey Position where
  fromJSONKey = FromJSONKeyTextParser parsePosition
    where
      parsePosition key = do
        let (x, y) = T.breakOn "-" key

        case mkPos <$> readMaybe (T.unpack x) <*> readMaybe (drop 1 $ T.unpack y) of
          Just pos -> return pos
          Nothing -> fail . T.unpack $ "Could not parse position: " <> x

mkPos x y = Position {xPos = x, yPos = y}

instance Hashable Position

data TileType
  = Letter Text
  | Blank
  deriving (Show, Eq)

instance Data.Aeson.ToJSON TileType where
  toJSON Blank = Data.Aeson.toJSON ("" :: Data.Text.Text)
  toJSON (Letter x) = Data.Aeson.toJSON x

newtype TileId = TileId Int deriving (Show, Eq, Generic)

instance ToJSON TileId
instance FromJSON TileId
instance Hashable TileId

instance FromJSON PlayerId

data TileLocation = LocationBoard Position | LocationRack PlayerId | LocationBag deriving (Show, Eq)

instance ToJSON TileLocation where
  toJSON LocationBag = object [ "type" .= ("bag" :: String) ]
  toJSON (LocationBoard pos) = object ["type" .= ("board" :: String), "position" .= pos]
  toJSON (LocationRack pid) = object [ "type" .= ("rack" :: String), "player" .= pid ]

instance FromJSON TileLocation where
  parseJSON = withObject "TileLocation" $ \v -> do
    (t :: T.Text) <- v .: "type"

    case t of
      "bag" -> return LocationBag
      "board" -> LocationBoard <$> v .: "position"
      "rack" -> LocationRack <$> v .: "player"
      _ -> fail . show $ "Unimplemented TileLocation type: " <> t

data Tile = Tile
  { _tileLetter :: TileType
  , _tileScore :: Int
  , _tileId :: TileId
  , _tileLocation :: TileLocation
  }
  deriving stock (Show, Generic)
  deriving Data.Aeson.ToJSON via StripPrefix "_tile" Tile

instance Eq Tile where
  a == b = _tileId a == _tileId b

mkTile :: Int -> T.Text -> Int -> Tile
mkTile id letter score = Tile
  { _tileLetter = toLetter letter
  , _tileScore = score
  , _tileId = TileId id
  , _tileLocation = LocationBag
  }
  where
    toLetter "" = Blank
    toLetter l = Letter l

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
  , _spaceOccupant :: Maybe TileId
  } deriving (Eq, Show)

emptySpace = Space None Nothing

spaceOccupant :: Control.Lens.Lens' Space (Maybe TileId)
spaceOccupant f parent =
  fmap (\x -> parent {_spaceOccupant = x}) (f (_spaceOccupant parent))

spaceBonus :: Control.Lens.Lens' Space Bonus
spaceBonus f parent =
  fmap (\x -> parent {_spaceBonus = x}) (f (_spaceBonus parent))

data Player = Player
  { --_playerRack :: [Tile]
    _playerScore :: Int
  , _playerName :: Text
  , _playerId :: PlayerId
  }
  deriving stock (Show, Generic)
  deriving Data.Aeson.ToJSON via StripPrefix "_player" Player
  deriving Eq

mkPlayer id name = Player
  { --_playerRack = mempty
    _playerScore = 0
  , _playerName = name
  , _playerId = id
  }

newtype PlayerId =
  PlayerId Integer
  deriving stock (Show, Eq, Generic)

instance Data.Aeson.ToJSON PlayerId
instance Hashable PlayerId

-- TODO: Need to move PlayerId outside this type, so that API FromJSON etc can
-- work nicely.
data Move =
  PlayTiles [Tile]
  deriving stock (Show, Generic)

type Board = M.HashMap Position Space
type PlayerMap = M.HashMap PlayerId Player
type TileMap = M.HashMap TileId Tile

data Game = Game
  { _gameBoard :: Board
  --, _gameBag :: [Tile]
  , _gamePlayers :: PlayerMap
  -- TODO: Store all tiles in here
  , _gameTiles :: TileMap
  , _gameVersion :: Int32
  } deriving (Show, Eq, Generic)

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

--gameBag :: Control.Lens.Lens' Game [Tile]
--gameBag f parent = fmap (\x -> parent {_gameBag = x}) (f (_gameBag parent))

gamePlayers :: Control.Lens.Lens' Game PlayerMap
gamePlayers f parent =
  fmap (\x -> parent {_gamePlayers = x}) (f (_gamePlayers parent))

gameTiles :: Control.Lens.Lens' Game TileMap
gameTiles f parent =
  fmap (\x -> parent {_gameTiles = x}) (f (_gameTiles parent))

gameVersion :: Control.Lens.Lens' Game Int32
gameVersion f parent =
  fmap (\x -> parent {_gameVersion = x}) (f (_gameVersion parent))

tileScore :: Control.Lens.Lens' Tile Int
tileScore f parent =
  fmap (\x -> parent {_tileScore = x}) (f (_tileScore parent))

tileLetter :: Control.Lens.Lens' Tile TileType
tileLetter f parent =
  fmap (\x -> parent {_tileLetter = x}) (f (_tileLetter parent))

tileLocation :: Control.Lens.Lens' Tile TileLocation
tileLocation f parent =
  fmap (\x -> parent {_tileLocation = x}) (f (_tileLocation parent))

tileId :: Control.Lens.Lens' Tile TileId
tileId f parent =
  fmap (\x -> parent {_tileId = x}) (f (_tileId parent))

playerId :: Control.Lens.Lens' Player PlayerId
playerId f parent =
  fmap (\x -> parent {_playerId = x}) (f (_playerId parent))

--playerRack :: Control.Lens.Lens' Player [Tile]
--playerRack f parent =
--  fmap (\x -> parent {_playerRack = x}) (f (_playerRack parent))

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
    Just (TileId id) -> " " <> T.pack (show id) <> " "

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
    --, _gameBag = mempty
    , _gamePlayers = mempty
    , _gameTiles = mempty
    , _gameVersion = 0
    }
