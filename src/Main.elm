import Browser
import Html exposing (Html, pre, text, ul, li)
-- import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, list, string)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GotBots (Result Http.Error (List String))


type Model
    = Failure
    | Loading
    | Success (List String)


init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:3000/robots"
        , expect = Http.expectJson GotBots botDecoder
        }
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotBots result ->
            case result of
                Ok bots ->
                    (Success bots, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)
                

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to get the robots"

        Loading ->
            text "Loading..."

        Success bots ->
            ul []
                (List.map (\botName -> li [] [ text botName ]) bots)


botDecoder : Decoder (List String)
botDecoder =
    list string
