port module Ports exposing (importQuestion, onSessionChange, storeSession)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port importQuestion : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg
