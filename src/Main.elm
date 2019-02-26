import Browser exposing (Document)
import Browser.Navigation as Nav

import Bytes exposing (Bytes)
import Task
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


type BotData
    = NotSelected
    | Selected File
    | Uploaded Bytes


type alias Model =
    { robots : List Robot
    , message : Message
    , uploadedBot : BotData
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model = { robots = []
                , message = Status NotStarted
                , uploadedBot = NotSelected
                }
    in
        (model, getRobots GotRobots)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotMessage (Maybe Message)
    | GotRobots (Result Http.Error (List Robot))
    | GotFile File
    | GotFileBytes Bytes
    | FileRequest
    | BotUploaded (Result Http.Error ())



uploadBot bytes =
    Http.post
        { url = "http://localhost:3000/upload"
        , body = Http.bytesBody "application/java-archive" bytes
        , expect = Http.expectWhatever BotUploaded
        }


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
            ( { model | uploadedBot = Selected file }, Task.perform GotFileBytes (File.toBytes file) )

        GotFileBytes bytes ->
            ( { model | uploadedBot = Uploaded bytes }, uploadBot bytes )

        FileRequest ->
            ( model, Select.file ["application/java-archive"] GotFile )

        BotUploaded result ->
            let
                _ = Debug.log <| Debug.toString result

            in
                ( { model | uploadedBot = NotSelected }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    recieveMessage GotMessage


showRobot : Robot -> Html msg
showRobot robot =
    li []
        [ text robot.name
        , button [ css [ marginLeft (px 20) ] ] [ text "download" ]
        ]


viewBotUpload : BotData -> Html Msg
viewBotUpload botData =
    case botData of
        NotSelected ->
            button [ onClick FileRequest ] [ text "Upload Bot" ]

        Selected file ->
            div [] [ text ("selected " ++ File.name file) ]

        Uploaded bytes ->
            div [] [ text "uploaded"]


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
