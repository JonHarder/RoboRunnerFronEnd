module Components.FileInput exposing (fileInput)

import Css exposing (..)
import File exposing (File)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id, css, for, name, type_, multiple)
import Html.Styled.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)


fileDecoder : Decoder File
fileDecoder =
    Decode.at ["target", "file"] File.decoder


fileInput : String -> (File -> msg) -> Html msg
fileInput message toMsg =
    div []
        [ input [ css
                  [ width (px 0.1)
                  , height (px 0.1)
                  , opacity (num 0)
                  , overflow hidden
                  , position absolute
                  , zIndex (int -1)
                  ]
                , on "change" (Decode.map toMsg fileDecoder)
                , type_ "file"
                , multiple False
                , id "file-upload"
                ]
              [ ]
        , label [ css
                  [ backgroundColor (rgb 0 0 0)
                  , color (rgb 0 255 0)
                  , display inlineBlock
                  , border3 (px 2) solid (rgb 0 255 0)
                  , padding (px 3)
                  , cursor pointer
                  ]
                , for "file-upload"
                ]
            [ text message ]
        ]
