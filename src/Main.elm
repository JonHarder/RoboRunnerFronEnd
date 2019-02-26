import Browser exposing (Document)
import Browser.Navigation as Nav

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Attributes exposing (css)
import File exposing (File)
import File.Select as Select

import Http
import Url

import Api.Robots exposing (getRobots, Robot)
import Websockets exposing (Message(..), Status(..), recieveMessage, showMessage)


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
    , message : Message
    , uploadedBot : Maybe File
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model = { robots = []
                , message = Status NotStarted
                , uploadedBot = Nothing
                }
    in
        (model, getRobots GotRobots)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotMessage (Maybe Message)
    | GotRobots (Result Http.Error (List Robot))
    | GotFile File
    | FileRequest


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

        GotMessage maybeMessage ->
            case maybeMessage of
                Just message ->
                    ( { model | message = message }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotFile file ->
            ( { model | uploadedBot = Just file }, Cmd.none )

        FileRequest ->
            ( model, Select.file ["application/java-archive"] GotFile )


subscriptions : Model -> Sub Msg
subscriptions model =
    recieveMessage GotMessage


showRobot : Robot -> Html msg
showRobot robot =
    li []
        [ text robot.name
        , button [ css [ marginLeft (px 20) ] ] [ text "download" ]
        ]


viewBotUpload : Maybe File -> Html Msg
viewBotUpload mBot =
    case mBot of
        Just bot ->
            div [] [ text ("got " ++ File.name bot) ]

        Nothing ->
            button [ onClick FileRequest ] [ text "Upload Bot" ]


styledView : Model -> Html Msg
styledView model = 
    div [ ]
        [ h1 [] [text "Robots"]
        , ul [] (List.map showRobot model.robots)
        , viewBotUpload model.uploadedBot
        , h1 [] [ text "Battle Results" ]
        , showMessage model.message
        ]


view : Model -> Document Msg
view model =
    { title = "Robo Runner"
    , body = [ toUnstyled <| styledView model ]
    }
