module BattleResults exposing (BattleResults, decodeBattleResults, showBattleResults)


import Html.Styled exposing (..)
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



showResult : BattleResult -> List (Html msg)
showResult battleResult =
    [ tr []
         [ td [] [ text "1st" ]
         , td [] [ text battleResult.first.name]
         , td [] [ text <| String.fromInt battleResult.first.score]
         ]
    , tr []
        [ td [] [ text "2nd" ]
        , td [] [ text battleResult.second.name]
        , td [] [ text <| String.fromInt battleResult.second.score]
        ]
    , hr [] []
    ]



showBattleResults : BattleResults -> Html msg
showBattleResults battleResults =
    let
        tableHead = 
            thead []
                  [ th [] [ text "Place" ]
                  , th [] [ text "Robot" ]
                  , th [] [ text "Score" ]
                  ]
        rows = List.concatMap showResult battleResults
    in
        table [] (tableHead :: rows)
        
