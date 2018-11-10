module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode
import Model exposing (..)
import Route exposing (..)
import Url
import Url.Builder


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Browser.Navigation.Key
    , route : Route
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    -- TODO Parse URL here
    let
        route =
            parseRoute url
    in
    ( Model key route, Cmd.none )


type Msg
    = Noop
    | MoveSubmit String
    | GameCreate
    | GameCreated (Result Http.Error GameId)
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log (Debug.toString msg) msg
    in
    case msg of
        GameCreate ->
            ( model, Http.send GameCreated (Http.post (Url.Builder.crossOrigin "http://localhost:8080" [ "game" ] []) Http.emptyBody gameIdDecoder) )

        GameCreated (Result.Ok gameId) ->
            ( model, Browser.Navigation.pushUrl model.key (urlFor (Game gameId)) )

        GameCreated (Result.Err _) ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | route = parseRoute url }, Cmd.none )

        MoveSubmit _ ->
            ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.route of
        NotFound ->
            { title = "Licross"
            , body =
                [ h1 [] [ text "Licross" ]
                , text "The requested page does not exist"
                ]
            }

        Home ->
            { title = "Licross"
            , body =
                [ h1 [] [ text "Licross" ]
                , ul []
                    [ li [] [ a [ href (urlFor Example) ] [ text "View example board" ] ]
                    , li [] [ a [ href (urlFor NewGame) ] [ text "New game" ] ]
                    ]
                ]
            }

        NewGame ->
            { title = "New Game - Licross"
            , body =
                [ h1 [] [ text "Licross" ]
                , button [ onClick GameCreate ] [ text "Create Game" ]
                ]
            }

        Game id ->
            { title = "Playing Game - Licross"
            , body =
                [ h1 [] [ text "Licross" ]
                , text (gameIdToString id)
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
