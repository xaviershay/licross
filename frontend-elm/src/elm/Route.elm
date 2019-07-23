module Route exposing (Route(..), parseRoute, subscriptionUrlFor, urlFor)

import Model exposing (..)
import Url
import Url.Builder
import Url.Parser exposing (..)


type Route
    = Home
    | Example
    | NewGame
    | Game GameId
    | NotFound


parseRoute : Url.Url -> Route
parseRoute url =
    Maybe.withDefault NotFound (parse routeParser url)


subscriptionUrlFor : GameId -> String
subscriptionUrlFor id =
    Url.Builder.absolute [ "game", gameIdToString id, "player", "123", "subscribe" ] []


urlFor : Route -> String
urlFor route =
    Url.Builder.absolute (partsFor route) []


partsFor route =
    case route of
        Game id ->
            [ "game", gameIdToString id ]

        NewGame ->
            [ "game", "new" ]

        Example ->
            [ "example" ]

        _ ->
            []


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Example (s "example")
        , map NewGame (s "game" </> s "new")
        , map Game (s "game" </> map GameId string)
        ]
