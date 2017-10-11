module Data.User exposing (User, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Data.AuthToken as AuthToken exposing (AuthToken)

type alias User =
    { email : String
    , token : AutoToken
    , username : String
    }


decoder : Decoder User
decoder =
    decode User
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "username" Decode.string


encode : User -> Value
encode user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "token", AuthToken.encode user.token )
        , ( "username", Encode.string user.username )
        ]
