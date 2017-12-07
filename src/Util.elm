module Util exposing ((=>), appendErrors, httpErrorToString, httpPost, maybeJoin, onClickStopPropagation, pair, viewIf)

import Config
import Html exposing (Attribute, Html)
import Html.Events exposing (defaultOptions, onWithOptions)
import Http
import Json.Decode as Decode
import Json.Encode


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>


{-| Useful when building up a Cmd via a pipeline, and then pairing it with
a model at the end.
session.user
|> User.Request.foo
|> Task.attempt Foo
|> pair { model | something = blah }
-}
pair : a -> b -> ( a, b )
pair first second =
    first => second


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    onWithOptions "click"
        { defaultOptions | stopPropagation = True }
        (Decode.succeed msg)


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }


httpPost : String -> payload -> (payload -> Json.Encode.Value) -> Decode.Decoder response -> (Result Http.Error response -> msg) -> Cmd msg
httpPost endpoint payload payloadEncoder responseDecoder responseMsg =
    let
        url =
            Config.server ++ "/" ++ endpoint

        body =
            Http.jsonBody <| payloadEncoder payload

        request =
            Http.post url body responseDecoder
    in
    Http.send responseMsg request


httpErrorToString : Http.Error -> String
httpErrorToString httpError =
    case httpError of
        Http.BadUrl str ->
            "Bad url: " ++ str

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Network error (no connectivity)."

        Http.BadStatus response ->
            "Bad status code returned: " ++ Basics.toString response.status.code

        Http.BadPayload debug_str response ->
            "JSON decoding of response failed: " ++ debug_str


maybeJoin : Maybe (Maybe a) -> Maybe a
maybeJoin mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing
