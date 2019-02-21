import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, text, li, ul)
import Http
import Html.Attributes exposing (href)
import Json.Decode exposing (Decoder)
import Url

import Api.Robots exposing (getRobots, Robot)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Model
    = Robots (List Robot)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model = Robots []
    in
        (model, getRobots GotRobots)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotRobots (Result Http.Error (List Robot))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        GotRobots response ->
            case response of
                Err _ ->
                    ( model, Cmd.none )

                Ok robots ->
                    ( Robots robots, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


showRobot : Robot -> Html msg
showRobot robot =
    li [] [ text robot.name ]


view : Model -> Document Msg
view (Robots robots) =
    { title = "Robo Runner"
    , body =
        [ div []
              [ h1 [] [text "Robots"]
              , ul [] (List.map showRobot robots)
              ]
        ]
    }
