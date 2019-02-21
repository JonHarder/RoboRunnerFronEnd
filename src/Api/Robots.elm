module Api.Robots exposing (getRobots, Robot)

import Http
import Json.Decode as Decode
import Json.Decode exposing (Decoder, list, string)

type alias Robot = { name : String }


parseRobots = list (Decode.map Robot string)


getRobots : (Result Http.Error (List Robot) -> msg) -> Cmd msg
getRobots mkMsg = Http.get
            { url = "http://localhost:3000/robots"
            , expect = Http.expectJson mkMsg parseRobots
            }
