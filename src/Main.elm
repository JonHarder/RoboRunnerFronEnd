import Dict exposing (Dict)
import Browser
import Html exposing (Html, button, div, h1, p, text, ul, li)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode exposing (Decoder, dict, int, list, string)


type Bot = Bot String


type Standings = Standings (Dict String (Dict String Int))


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
    | GotStandings (Result Http.Error Standings)
    | StartBattle BaseArgs
    | BattleStarted (Result Http.Error ())
    | BackToBots BaseArgs


type alias BaseArgs =
    { bots : List Bot }


type Model
    = Failure
    | Loading
    | Base BaseArgs
    | LoadStandings BaseArgs
    | ShowStandings Standings


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

        GotStandings result ->
            case result of
                Ok data ->
                    (ShowStandings data, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)
                
        StartBattle data ->
            (LoadStandings data, Http.post
                 { body = Http.emptyBody
                 , url = "http://localhost:3000/battles"
                 , expect = Http.expectWhatever BattleStarted
                 })

        BattleStarted result ->
            case result of
                Ok _ ->
                    (model, Http.get
                         { url = "http://localhost:3000/standings"
                         , expect = Http.expectJson GotStandings standingsDecoder
                         }
                    )

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

        LoadStandings data ->
            div []
                [ h1 [] [ text "Bot Standings" ]
                , button [ onClick (BackToBots data) ] [ text "Back" ]
                ]

        ShowStandings (Standings data) ->
            div []
                [ h1 [] [ text "Standings" ]
                , p [] [ text (String.fromInt (Dict.size data)) ]
                ]



standingsDecoder : Decoder Standings
standingsDecoder = Decode.map Standings (dict (dict int))


botDecoder : Decoder (List Bot)
botDecoder =
    list (Decode.map Bot string)
