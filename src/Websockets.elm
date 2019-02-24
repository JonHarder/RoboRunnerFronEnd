port module Websockets exposing (Message(..), Status(..), BattleResults, recieveMessage, showMessage)

import Components.Progress exposing (Progress, decodeProgress, progressBar)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode
import Json.Decode exposing (Value, Decoder, andThen, fail, field, decodeValue, succeed, list, string, int)
import Json.Decode.Pipeline exposing (required, hardcoded)


port webSocketRecieveMessage : (Value -> msg) -> Sub msg


recieveMessage : (Maybe Message -> msg) -> Sub msg
recieveMessage toMsg =
    webSocketRecieveMessage (\val -> toMsg (getMessage val))


getMessage : Value -> Maybe Message
getMessage jsonMessage =
    decodeValue decodeMessage jsonMessage
        |> Result.toMaybe


type alias BotAndScore =
    { name : String
    , score : Int
    }

type alias BattleResult =
    { first : BotAndScore
    , second : BotAndScore
    }

type alias BattleResults = List BattleResult


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


decodeBotAndScore : Decoder BotAndScore
decodeBotAndScore =
    succeed BotAndScore
        |> required "name" string
        |> required "score" int


decodeBattle : Decoder BattleResult
decodeBattle =
    succeed BattleResult
        |> required "first" decodeBotAndScore
        |> required "second" decodeBotAndScore


decodeBattleResults : Decoder BattleResults
decodeBattleResults =
    field "battles" (list decodeBattle)


type Status = NotStarted | Started | Finished


type Message
    = Status Status
    | Results BattleResults
    | InProgress Progress



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
