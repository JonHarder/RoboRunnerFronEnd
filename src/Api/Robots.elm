module Api.Robots exposing (downloadRobot, getRobots, Robot)

import Http
import File.Download as Download
import Json.Decode as Decode
import Json.Decode exposing (Decoder, list, string)

type alias Robot = { name : String }


parseRobots = list (Decode.map Robot string)


getRobots : (Result Http.Error (List Robot) -> msg) -> Cmd msg
getRobots mkMsg = Http.get
            { url = "http://localhost:3000/robots"
            , expect = Http.expectJson mkMsg parseRobots
            }


downloadRobot : Robot -> Cmd msg
downloadRobot robot =
    Download.url <| "http://localhost:3000/download/" ++ robot.name ++ ".jar"

