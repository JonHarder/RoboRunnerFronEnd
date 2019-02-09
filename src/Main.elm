import Browser
import Html exposing (Html, button, div, pre, text, ul, li)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode exposing (Decoder, list, string)


type Bot = Bot String


displayBot : Bot -> String
displayBot (Bot name) = "Bot: " ++ name
 

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GotBots (Result Http.Error (List Bot))
    | StartBattle
    | BattleStarted (Result Http.Error ())


type Model
    = Failure
    | Loading
    | Success (List Bot)


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
                
        StartBattle ->
            (model, Http.post
                 { body = Http.emptyBody
                 , url = "http://localhost:3000/battles"
                 , expect = Http.expectWhatever BattleStarted
                 })

        BattleStarted result ->
            case result of
                Ok _ ->
                    (model, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)


viewBot : Bot -> Html msg
viewBot bot =
    li [] [ text (displayBot bot) ]


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to get the robots"

        Loading ->
            text "Loading..."

        Success bots ->
            div []
                [ ul [] <| List.map viewBot bots
                , button [ onClick StartBattle ] [ text "Battle!" ]
                ]


botDecoder : Decoder (List Bot)
botDecoder =
    list (Decode.map Bot string)
