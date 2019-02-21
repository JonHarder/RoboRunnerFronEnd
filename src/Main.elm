import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, h2, text, li, ul)
import Http
import Html.Attributes exposing (href)
import Json.Decode exposing (Decoder)
import Url

import Api.Robots exposing (getRobots, Robot)
import Websockets exposing (BattleResults, showBattleResults, recieveBattleResults)


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


type alias Model =
    { robots : List Robot
    , battleResults : Maybe BattleResults
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model = { robots = []
                , battleResults = Nothing
                }
    in
        (model, getRobots GotRobots)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotRobots (Result Http.Error (List Robot))
    | GotBattleResults (Maybe BattleResults)


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
                    ( { model | robots = robots }, Cmd.none )

        GotBattleResults results ->
            ( { model | battleResults = results }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    recieveBattleResults GotBattleResults


showRobot : Robot -> Html msg
showRobot robot =
    li [] [ text robot.name ]


view : Model -> Document Msg
view model =
    { title = "Robo Runner"
    , body =
          [ div []
                [ h1 [] [text "Robots"]
                , ul [] (List.map showRobot model.robots)
                , case model.battleResults of
                      Just results ->
                          showBattleResults results

                      Nothing ->
                          div [] []
                ]
          ]
    }
