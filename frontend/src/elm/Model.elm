module Model exposing (GameId(..), gameIdDecoder, gameIdToString)

import Json.Decode


type GameId
    = GameId Int


gameIdToString (GameId id) =
    String.fromInt id


gameIdDecoder =
    Json.Decode.map GameId Json.Decode.int
