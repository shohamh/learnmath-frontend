module Forms.Register exposing (..)

import Html exposing (..)
import Http
import Material
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css, when, onClick, onInput)
import Material.Textfield as Textfield
import Json.Decode
import Json.Encode


type alias Model =
    { username : String
    , password : String
    , passwordAgain : String
    , email : String
    }


model : Model
model =
    { username = ""
    , password = ""
    , passwordAgain = ""
    , email = ""
    }


type alias RequestData =
    { username : String
    , password : String
    , email : String
    }


type alias ResponseData =
    { success : Bool
    , error_messages : List String
    }


type Msg
    = UpdatePassword String
    | UpdatePasswordAgain String
    | Submit


requestModel : Model -> RequestData
requestModel model =
    RequestData model.username model.password model.email


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePassword str ->
            { model | password = str } ! []

        UpdatePasswordAgain str ->
            { model | passwordAgain = str } ! []

        Submit ->
            let
                requestData =
                    requestModel model
            in
                model ! [ send requestData ]


send : RequestData -> Cmd Msg
send requestData =
    let
        url =
            "http://learnmath.pythonanywhere.com/register"
                Http.jsonBody
                Json.Encode.object
                [ ( "username", Json.Encode.string requestData.username )
                , ( "password", Json.Encode.string requestData.password )
                , ( "email", Json.Encode.string requestData.email )
                ]

        request =
            Http.post url decodeResult
    in
        Http.send ResponseData request


decodeResult : Json.Decode.Decoder ResponseData
decodeResult =
    Json.Decode.map2 ResponseData
        (Json.Decode.field "success" Json.Decode.bool)
        (Json.Decode.field
            "error_messages"
            Json.Decode.string
        )


type alias Mdl =
    Material.Model


registerForm : Model -> Html Msg
registerForm model =
    div []
        [ Textfield.render Material.Model
            [ 0 ]
            model.mdl
            [ Textfield.label "Username"
            , Textfield.floatingLabel
            , Textfield.text_
            ]
            []
        , Textfield.render Material.Model
            [ 1 ]
            model.mdl
            [ Textfield.label "Password"
            , Textfield.floatingLabel
            , Textfield.password
            , Options.onInput UpdatePassword
            , Textfield.error ("Passwords don't match.") |> Options.when (model.register_password /= model.register_passwordAgain)
            ]
            []
        , Textfield.render Material.Model
            [ 2 ]
            model.mdl
            [ Textfield.label "Confirm password"
            , Textfield.floatingLabel
            , Textfield.password
            , Options.onInput UpdatePasswordAgain
            , Textfield.error ("Passwords don't match.") |> Options.when (model.register_password /= model.register_passwordAgain)
            ]
            []
        , Textfield.render Material.Model
            [ 3 ]
            model.mdl
            [ Textfield.label "Email"
            , Textfield.floatingLabel
            , Textfield.email
            ]
            []
        , Button.render Material.Model
            [ 4 ]
            model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Options.onClick Submit
            ]
            [ text "Register" ]
        ]
