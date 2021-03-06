{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    -- TODO: Redo these defaults after regen-ing template
    id <- v .:? "id" .!= 0
    location <- v .:? "location" .!= LocationBag

    return $ set tileLocation location (mkTile id letter score)

instance ToJSON Space where
  toJSON = toJSON . view spaceBonus

instance ToJSON RedactedGame where
  toJSON (RedactedGame Nothing x) =
    -- TODO: Redact tiles in racks/bag
    object
      [ "board" .= view gameBoard x
      , "tiles" .= (M.elems $ view gameTiles x)
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
    tiles :: [Tile] <- v .: "tiles"
    board :: M.HashMap Position Space <- v .: "board"

    return
      . set gameBoard board
       . set gameTiles (M.fromList $ map (\x -> (view tileId x, x)) tiles)
      $ emptyGame

instance FromJSON Space where
  parseJSON = withText "Bonus" $ \v -> do
    bonus <-
      case v of
        "none" -> return None
        "dw" -> return $ WordMultiplier 2
        "tw" -> return $ WordMultiplier 3
        "dl" -> return $ LetterMultiplier 2
        "tl" -> return $ LetterMultiplier 3
        "anchor" -> return Anchor
        _ -> fail . show $ "Unknown bonus type: " <> v

    return . set spaceBonus bonus $ emptySpace

--instance ToJSON FlattenSpace where
--  toJSON (FlattenSpace (pos, space)) =
--    object $
--    ["x" .= xPos pos, "y" .= yPos pos, "bonus" .= view spaceBonus space] <>
--    maybe mempty tileFields (view spaceOccupant space)
--    where
--      tileFields (Tile text tile) =
--        ["letter" .= text, "score" .= view tileScore tile]

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
instance ToJSONKey TileId

instance FromJSON Move where
  parseJSON = withObject "Move" $ \v -> do
    t <- v .: "type"

    case t of
      ("PlayTiles" :: T.Text) -> PlayTiles <$> v .: "tiles"
