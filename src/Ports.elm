port module Ports exposing (importQuestion, myscriptConvert, onSessionChange, storeSession)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port importQuestion : ( String, Int ) -> Cmd msg


port myscriptConvert : () -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg
