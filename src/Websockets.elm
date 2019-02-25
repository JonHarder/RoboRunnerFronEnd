port module Websockets exposing (Message(..), Status(..), recieveMessage, showMessage)

import Components.Progress exposing (Progress, decodeProgress, progressBar)
import BattleResults exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode
import Json.Decode exposing (Value, Decoder, andThen, fail, field, decodeValue, succeed, list, string, int)
import Json.Decode.Pipeline exposing (required, hardcoded)


type Status = NotStarted | Started | Finished


type Message
    = Status Status
    | Results BattleResults
    | InProgress Progress


port webSocketRecieveMessage : (Value -> msg) -> Sub msg


recieveMessage : (Maybe Message -> msg) -> Sub msg
recieveMessage toMsg =
    webSocketRecieveMessage (\val -> toMsg (getMessage val))


getMessage : Value -> Maybe Message
getMessage jsonMessage =
    decodeValue decodeMessage jsonMessage
        |> Result.toMaybe


showMessage : Message -> Html msg
showMessage message =
    case message of
        Status NotStarted ->
            div [] [ text "battle not started" ]

        Status Started ->
            div [] [ text "battle started!" ]

        Status Finished ->
            div [] [ text "battle finished!" ]

        Results _ ->
            div [] [ text "got some battle results" ]

        InProgress p ->
            div []
                [ div [ css
                        [ textAlign center]
                      ]
                      [ text "Running..." ]
                 , progressBar p
                ]


messageDispatch : String -> Decoder Message
messageDispatch message =
    case message of
        "started" ->
            succeed (Status Started)

        "finished" ->
            succeed (Status Finished)

        "results" ->
            field "data" (Decode.map Results decodeBattleResults)

        "progress" ->
            field "data" (Decode.map InProgress decodeProgress)

        _ ->
            fail "unknown message type"


decodeMessage : Decoder Message
decodeMessage =
    field "message" string
        |> andThen messageDispatch
