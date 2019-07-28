{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

-- ToJSON instances are always exported, don't export anything else.
module Licross.Json ( ) where

-- base
import GHC.Generics

-- aeson
import           Data.Aeson

-- unordered-containers
import qualified Data.HashMap.Strict as M

-- text
import qualified Data.Text as T

-- licross
import           Licross.Prelude
import           Licross.Types
import           Licross.Extras.Aeson

newtype FlattenSpace =
  FlattenSpace (Position, Space)

instance FromJSON Tile where
  parseJSON = withObject "Tile" $ \v -> do
    letter <- v .: "letter"
    score <- v .: "score"
    id <- v .:? "id" .!= 0

    return $ mkTile id letter score

instance ToJSON RedactedGame where
  toJSON (RedactedGame Nothing x) =
    object
      [ "board" .= map FlattenSpace (M.toList $ view gameBoard x)
      , "bag" .= view gameBag x
      , "players" .= (M.elems $ view gamePlayers x)
      ]

data FlattenedSpace = FlattenedSpace Bonus Integer Integer

instance FromJSON FlattenedSpace where
  parseJSON = withObject "FlattenedSpace" $ \v -> do
    FlattenedSpace <$> v .: "bonus" <*> v .: "x" <*> v .: "y"

-- When loading a full Game from JSON, ignore IDs. This is used currently when
-- loading from disk, depending on how persistence ends up happening we may
-- want to reverse this decision.
data TileNoId = TileNoId
  { _tileNoIdLetter :: T.Text
  , _tileNoIdScore :: Int
  }
  deriving stock (Generic)
  deriving FromJSON via StripPrefix "_tileNoId" TileNoId

instance FromJSON Game where
  parseJSON = withObject "Game" $ \v -> do
    bagNoIds <- v .: "bag"
    boardSpec <- v .: "board"

    let bag = map buildTile $ zip bagNoIds [1..]

    return
      . set gameBoard (M.fromList $ map (\(FlattenedSpace bonus x y) -> (mkPos x y, set spaceBonus bonus emptySpace)) boardSpec)
      . set gameBag bag
      $ emptyGame

    where
      buildTile (t, id) = mkTile id (_tileNoIdLetter t) (_tileNoIdScore t)

instance ToJSON FlattenSpace where
  toJSON (FlattenSpace (pos, space)) =
    object $
    ["x" .= xPos pos, "y" .= yPos pos, "bonus" .= view spaceBonus space] <>
    maybe mempty tileFields (view spaceOccupant space)
    where
      tileFields (PlacedTile text tile) =
        ["letter" .= text, "score" .= view tileScore tile]

instance ToJSON Bonus where
  toJSON None = jsonString "none"
  toJSON (WordMultiplier 2) = jsonString "dw"
  toJSON (WordMultiplier 3) = jsonString "tw"
  toJSON (LetterMultiplier 2) = jsonString "dl"
  toJSON (LetterMultiplier 3) = jsonString "tl"
  toJSON Anchor = jsonString "anchor"
  toJSON x = jsonString "unimplemented"

jsonString :: String -> Data.Aeson.Value
jsonString x = toJSON x

instance FromJSON Bonus where
  parseJSON = withText "Bonus" $ \v ->
    return $
      case v of
        "none" -> None
        "dw" -> WordMultiplier 2
        "tw" -> WordMultiplier 3
        "dl" -> LetterMultiplier 2
        "tl" -> LetterMultiplier 3
        "anchor" -> Anchor

instance ToJSON GameId
instance ToJSONKey PlayerId

instance FromJSON Move where
  parseJSON = withObject "Move" $ \v -> do
    pure (PlayTiles mempty)
