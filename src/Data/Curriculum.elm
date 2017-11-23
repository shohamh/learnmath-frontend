module Data.Curriculum exposing (Curriculum, decoder, encode)

import Data.Subject exposing Subject
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Curriculum
    = Curriculum (List Subject)


encode : Curriculum -> Value
encode (Curriculum subjectList) =
    Encode.object [
        ("subjects", Encode.list (List.map Subject.encode subjectList))
    ]


decoder : Decoder Subject
decoder =
    Decode.string
        |> Decode.map AuthToken
