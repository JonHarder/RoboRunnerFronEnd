port module Websockets exposing (BattleResults, showBattleResults, recieveBattleResults)

import Html exposing (Html, text, ul, li)
import Json.Decode as Decode
import Json.Decode exposing (Value, Decoder, field, decodeValue, succeed, list, string, int)
import Json.Decode.Pipeline exposing (required)


port recieveMessage : (Value -> msg) -> Sub msg


recieveBattleResults : (Maybe BattleResults -> msg) -> Sub msg
recieveBattleResults toMsg =
    recieveMessage (\val -> toMsg (getBattleResults val))


getBattleResults : Value -> Maybe BattleResults
getBattleResults jsonBattleResults =
    decodeValue decodeBattleResults jsonBattleResults
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


showBattleResult : BattleResult -> Html msg
showBattleResult battleResult =
    li [] [ ul []
                [ li [] [ text <| "first: " ++ battleResult.first.name ++ ": " ++ String.fromInt battleResult.first.score ]
                , li [] [ text <| "second: " ++ battleResult.second.name ++ ": " ++ String.fromInt battleResult.second.score ]
                ]
          ]


showBattleResults : BattleResults -> Html msg
showBattleResults battleResults =
    ul [] (List.map showBattleResult battleResults)


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
