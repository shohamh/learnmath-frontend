module Data.Session exposing (Session, attempt, getSid)

import Data.AuthToken exposing (AuthToken)
import Data.User as User exposing (User)
import Util exposing ((=>))


type alias Session =
    { user : Maybe User }


getSid : Session -> String
getSid session =
    case session.user of
        Just user ->
            Data.AuthToken.toString user.token

        Nothing ->
            ""


attempt : String -> (AuthToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
    case Maybe.map .token session.user of
        Nothing ->
            [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ] => Cmd.none

        Just token ->
            [] => toCmd token
