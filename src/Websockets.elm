port module Websockets exposing (recieveMessage)


port recieveMessage : (String -> msg) -> Sub msg
