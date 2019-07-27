{-# LANGUAGE OverloadedStrings #-}

-- ToJSON instances are always exported, don't export anything else.
module Licross.Json ( ) where

-- aeson
import           Data.Aeson

-- unordered-containers
import qualified Data.HashMap.Strict as M

-- licross
import           Licross.Prelude
import           Licross.Types

newtype FlattenSpace =
  FlattenSpace (Position, Space)

instance ToJSON RedactedGame where
  toJSON (RedactedGame Nothing x) =
    object
      [ "board" .= map FlattenSpace (M.toList $ view gameBoard x)
      , "bag" .= view gameBag x
      , "players" .= view gamePlayers x
      ]

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
  toJSON (CompositeBonus a b) = error "unimplemented"

jsonString :: String -> Data.Aeson.Value
jsonString x = toJSON x

instance ToJSON PlayerId
instance ToJSON GameId

instance FromJSON Move where
  parseJSON = withObject "Move" $ \v -> do
    pure (PlayTiles (PlayerId 0) mempty)
