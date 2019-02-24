module Components.Progress exposing (Progress, decodeProgress, progressBar)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)

import Json.Decode exposing (Decoder, succeed, int)
import Json.Decode.Pipeline exposing (required)


type alias Progress = { complete : Int, outOf : Int}


decodeProgress : Decoder Progress
decodeProgress =
    succeed Progress
        |> required "complete" int
        |> required "out-of" int


progressBar : Progress -> Html msg
progressBar progress =
    let
        percent = 100 * (toFloat progress.complete / toFloat progress.outOf)
    in
        div [ css
              [ marginLeft (px 10)
              , width (pct 100)
              , border3 (px 5) solid (rgb 255 255 255)
              ]
            ]
            [ div [ css
                    [ width (pct percent)
                    , marginRight auto
                    , color (rgb 255 255 255)
                    , backgroundColor (rgb 0 100 0)
                    , textAlign center
                    ]
                  ]
                  [ text <| String.fromInt (Basics.round percent) ++ "%" ]
            ]

