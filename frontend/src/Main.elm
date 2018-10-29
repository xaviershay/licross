module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init flags =
    ( 0, Cmd.none )


type Msg
    = Noop
    | MoveSubmit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.node "licross-game-area"
        [ Html.Attributes.property "gameId" (Json.Encode.int 12345)
        , Html.Events.on "submitMove" <| Json.Decode.map MoveSubmit <| Json.Decode.string
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
