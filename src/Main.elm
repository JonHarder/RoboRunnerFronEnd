import Browser
import Html exposing (Html, button, div, h1, p, text, ul, li)
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
    | StartBattle BaseArgs
    | BattleStarted (Result Http.Error ())
    | BackToBots BaseArgs


type alias BaseArgs =
    { bots : List Bot }


type Model
    = Failure
    | Loading
    | Base BaseArgs
    | Standings BaseArgs


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
                    (Base { bots = bots }, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)
                
        StartBattle data ->
            (Standings data, Http.post
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

        BackToBots data ->
            (Base data, Cmd.none)


viewBot : Bot -> Html msg
viewBot bot =
    li [] [ text (displayBot bot) ]


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "something went wrong :("

        Loading ->
            text "Loading..."

        Base data ->
            div []
                [ ul [] <| List.map viewBot data.bots
                , button [ onClick (StartBattle data) ] [ text "Battle!" ]
                , p [] [ text "TODO: once battle is started, hit standings and refresh until theres something there" ]
                ]

        Standings data ->
            div []
                [ h1 [] [ text "Bot Standings" ]
                , button [ onClick (BackToBots data) ] [ text "Back" ]
                ]



botDecoder : Decoder (List Bot)
botDecoder =
    list (Decode.map Bot string)
