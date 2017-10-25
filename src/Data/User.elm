module Data.User exposing (Role(..), User, Username(..), decoder, encode, roleDecoder, roleEncoder, roleToString, usernameDecoder, usernameParser, usernameToHtml, usernameToString)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import UrlParser


type alias User =
    { email : String
    , token : AuthToken
    , username : Username
    , role : Role
    }


type Role
    = Student
    | Teacher


roleDecoder : Decoder Role
roleDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Student" ->
                        Decode.succeed Student

                    "Teacher" ->
                        Decode.succeed Teacher

                    somethingElse ->
                        Decode.fail <| "Unknown role: " ++ somethingElse
            )


roleEncoder : Role -> Value
roleEncoder role =
    case role of
        Student ->
            Encode.string "Student"

        Teacher ->
            Encode.string "Teacher"


decoder : Decoder User
decoder =
    decode User
        |> required "email" Decode.string
        |> required "token" AuthToken.decoder
        |> required "username" usernameDecoder
        |> required "role" roleDecoder


encode : User -> Value
encode user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "token", AuthToken.encode user.token )
        , ( "username", encodeUsername user.username )
        , ( "role", roleEncoder user.role )
        ]


type Username
    = Username String


usernameToString : Username -> String
usernameToString (Username username) =
    username


usernameParser : UrlParser.Parser (Username -> a) a
usernameParser =
    UrlParser.custom "USERNAME" (Ok << Username)


usernameDecoder : Decoder Username
usernameDecoder =
    Decode.map Username Decode.string


encodeUsername : Username -> Value
encodeUsername (Username username) =
    Encode.string username


usernameToHtml : Username -> Html msg
usernameToHtml (Username username) =
    Html.text username


roleToString : Role -> String
roleToString role =
    case role of
        Student ->
            "Student"

        Teacher ->
            "Teacher"
