module Model exposing (GameId(..), gameIdDecoder, gameIdToString)

import Json.Decode


type GameId
    = GameId String


gameIdToString (GameId id) =
    id


gameIdDecoder =
    Json.Decode.map GameId Json.Decode.string
