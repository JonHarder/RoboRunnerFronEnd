module BattleResults exposing (..)


import Json.Decode as Decode exposing (Decoder, field, int, string, succeed, list)
import Json.Decode.Pipeline exposing (required)


type alias BotAndScore =
    { name : String
    , score : Int
    }

type alias BattleResult =
    { first : BotAndScore
    , second : BotAndScore
    }

type alias BattleResults = List BattleResult



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
