module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events
import Json.Decode
import Json.Encode
import Url
import Url.Parser


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type Route
    = Home
    | Example
    | NotFound


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Example (Url.Parser.s "example")
        ]


type alias Model =
    { key : Browser.Navigation.Key
    , route : Route
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    -- TODO Parse URL here
    let
        route =
            Maybe.withDefault NotFound (Url.Parser.parse routeParser url)
    in
    ( Model key route, Cmd.none )


type Msg
    = Noop
    | MoveSubmit String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | route = Maybe.withDefault NotFound (Url.Parser.parse routeParser url) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.route of
        NotFound ->
            { title = "Licross"
            , body =
                [ h1 [] [ text "Licross" ]
                , text "The requeste page does not exist"
                ]
            }

        Home ->
            { title = "Licross"
            , body =
                [ h1 [] [ text "Licross" ]
                , a [ href "example" ] [ text "View example board" ]
                ]
            }

        Example ->
            { title = "Licross"
            , body =
                [ Html.node "licross-game-area"
                    [ Html.Attributes.property "gameId" (Json.Encode.int 12345)
                    , Html.Events.on "submitMove" <| Json.Decode.map MoveSubmit <| Json.Decode.string
                    ]
                    []
                ]
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
