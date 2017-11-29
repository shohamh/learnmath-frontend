module Request.User exposing (register, storeSession)

import Config exposing (apiUrl)
import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.User as User exposing (User, Username(..))
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Ports
import Util exposing ((=>))


storeSession : User -> Cmd msg
storeSession user =
    User.encode
        user
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession



{-
   login : { r | username : Username, password : String } -> Http.Request (Result { r | success : String, error_messages : List String } User)
   login { username, password } =
       let
           body =
               Encode.object
                   [ "username" => User.encodeUsername username
                   , "password" => Encode.string password
                   ]
                   |> Http.jsonBody
       in
       Decode.field "user" User.decoder
           |> Http.post (apiUrl "/login") body
-}


register : { r | username : Username, email : String, password : String } -> Http.Request User
register { username, email, password } =
    let
        user =
            Encode.object
                [ "username" => User.encodeUsername username
                , "email" => Encode.string email
                , "password" => Encode.string password
                ]

        body =
            Encode.object [ "user" => user ]
                |> Http.jsonBody
    in
    Decode.field "user" User.decoder
        |> Http.post (apiUrl "/users") body
