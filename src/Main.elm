import Browser exposing (Document)
import Browser.Navigation as Nav

import Bytes exposing (Bytes)
import Task
import Css exposing (..)
import Html
import Html.Styled as Styled
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Attributes exposing (css)
import File exposing (File)
import File.Select as Select

import Http
import Url

import Api.Robots exposing (downloadRobot, getRobots, Robot)
import Websockets exposing (Message(..), Status(..), recieveMessage, showMessage)
import Components.FileUpload as FileUpload


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
    , uploadState : FileUpload.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model = { robots = []
                , message = Status NotStarted
                , uploadState = FileUpload.init "application/java-archive" "http://localhost:3000/upload"
                }
    in
        (model, getRobots GotRobots)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotMessage (Maybe Message)
    | GotRobots (Result Http.Error (List Robot))
    | DownloadBot Robot
    | StartBattle
    | UploadMsg FileUpload.Msg
    | NoOp



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

        DownloadBot robot ->
            (model, downloadRobot robot)

        GotMessage maybeMessage ->
            case maybeMessage of
                Just message ->
                    case message of
                        BotUploaded ->
                            ( model, getRobots GotRobots)

                        _ ->
                            ( { model | message = message }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UploadMsg uploadMsg ->
            let
                (newModel, newMsg) = FileUpload.update uploadMsg model.uploadState
            in
                ( { model | uploadState = newModel }, Cmd.map UploadMsg newMsg )

        StartBattle ->
            ( model, startBattle )

        NoOp ->
            ( model, Cmd.none )


startBattle : Cmd Msg
startBattle =
    Http.post
        { url = "http://localhost:3000/battles"
        , body = Http.emptyBody
        , expect = Http.expectWhatever (\_ -> NoOp) }


subscriptions : Model -> Sub Msg
subscriptions model =
    recieveMessage GotMessage


showRobot : Robot -> Html Msg
showRobot robot =
    li [] [ text robot.name
          , button [ onClick (DownloadBot robot) ] [ text "Download" ] ]


styledView : Model -> Html Msg
styledView model = 
    div [ css
          [ marginRight auto
          , marginLeft auto
          , width (pct 75)
          ]
        ]
        [ h1 [] [text "Robots"]
        , ul [] (List.map showRobot model.robots)
        , Styled.map UploadMsg (FileUpload.upload model.uploadState)
        , h1 [] [ text "Battle Results" ]
        , showMessage StartBattle model.message
        ]


view : Model -> Document Msg
view model =
    { title = "Robo Runner"
    , body = [ toUnstyled <| styledView model ]
    }
