module Data.Subject exposing (Subject, decoder, encode, withAuthorization)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Subject
    = Subject String


encode : Subject -> Value
encode (Subject token) =
    Encode.string token


decoder : Decoder Subject
decoder =
    Decode.string
        |> Decode.map Subject
