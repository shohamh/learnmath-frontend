module Forms.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Http
import Material
import Material.Layout as Layout
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css, when, onClick, onInput)
import Material.Textfield as Textfield
import Json.Decode
import Json.Encode


type alias LoginFormData =
    { username : String
    , password : String
    }


type alias LoginRequestData =
    { username : String
    , password : String
    }


type alias LoginResponse =
    { success : Bool
    , error_messages : List String
    }


type Msg
    = Submit


sendRegister : RegisterRequestData -> Cmd Msg
sendRegister registerData =
    let
        url =
            "http://learnmath.pythonanywhere.com/register"
                Http.jsonBody
                Json.Encode.object
                [ ( "username", Json.Encode.string registerData.username )
                , ( "password", Json.Encode.string registerData.password )
                , ( "email", Json.Encode.string registerData.email )
                ]

        request =
            Http.post url decodeRegisterResult
    in
        Http.send RegistrationResult request


type alias JsonResult =
    { success : Bool
    , errorMessage : String
    }


decodeRegistrationResult : Json.Decode.Decoder JsonResult
decodeRegistrationResult =
    Json.Decode.map2 JsonResult
        (Json.Decode.field "success" Json.Decode.bool)
        (Json.Decode.field
            "error_messages"
            Json.Decode.string
        )


loginForm : Model -> Html Msg
loginForm model =
    div []
        [ Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.label "Username"
            , Textfield.floatingLabel
            , Textfield.text_
            ]
            []
        , Textfield.render Mdl
            [ 1 ]
            model.mdl
            [ Textfield.label "Password"
            , Textfield.floatingLabel
            , Textfield.password
            ]
            []
        , Button.render Mdl
            [ 2 ]
            model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Options.onClick Submit
            ]
            [ text "Login" ]
        ]
