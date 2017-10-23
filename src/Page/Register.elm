module Page.Register exposing (ExternalMsg(..), Model, Msg, model, update, view)

import Config
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (..)
import List
import Ports
import Route
import Util exposing ((=>), httpPost)
import Views.Form as Form
import Views.Page as Page


type alias Model =
    { username : String
    , password : String
    , passwordAgain : String
    , email : String
    , successMessage : String
    , errorMessages : List String
    }


model : Model
model =
    { username = ""
    , password = ""
    , passwordAgain = ""
    , email = ""
    , successMessage = ""
    , errorMessages = []
    }


type alias RequestData =
    { username : String
    , password : String
    , email : String
    }


type alias ResponseData =
    { success : Bool
    , errorMessages : List String
    }


type Msg
    = SetUsername String
    | SetPassword String
    | SetPasswordAgain String
    | SetEmail String
    | Register
    | RegisterResult (Result Http.Error ResponseData)


type ExternalMsg
    = NoOp
    | SetUser User


requestModel : Model -> RequestData
requestModel model =
    RequestData model.username model.password model.email


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        SetUsername str ->
            { model | username = str } => Cmd.none => NoOp

        SetPassword str ->
            { model | password = str } => Cmd.none => NoOp

        SetPasswordAgain str ->
            { model | passwordAgain = str } => Cmd.none => NoOp

        SetEmail str ->
            { model | email = str } => Cmd.none => NoOp

        Register ->
            model => httpPost "register" (requestModel model) requestEncoder responseDecoder RegisterResult => NoOp

        RegisterResult (Ok responseData) ->
            { model
                | successMessage =
                    case responseData.success of
                        True ->
                            "Registration was successful."

                        False ->
                            "Registration failed."
                , errorMessages = responseData.errorMessages
            }
                => Cmd.none
                => NoOp

        RegisterResult (Err httpError) ->
            let
                errorMessage =
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
            in
            { model
                | errorMessages =
                    List.append model.errorMessages
                        [ errorMessage
                        ]
            }
                => Cmd.none
                => NoOp


requestEncoder : RequestData -> JE.Value
requestEncoder requestData =
    JE.object
        [ ( "username", JE.string requestData.username )
        , ( "password", JE.string requestData.password )
        , ( "email", JE.string requestData.email )
        ]


responseDecoder : Decoder ResponseData
responseDecoder =
    JDP.decode ResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)


viewErrorMessages : List String -> Html Msg
viewErrorMessages errorMessages =
    div [] (List.intersperse (br [] []) (List.map text errorMessages))


view : Session -> Model -> Html Msg
view session model =
    div [ class "auth-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                    , p [ class "text-xs-center" ]
                        [ a [ Route.href Route.Login ]
                            [ text "Have an account?" ]
                        ]
                    , viewForm
                    , div
                        [ style
                            [ ( "font-size", "20px" )
                            ]
                        ]
                        [ text model.successMessage ]
                    , viewErrorMessages model.errorMessages

                    -- , Form.viewErrors model.errorMessages
                    ]
                ]
            ]
        ]


viewForm : Html Msg
viewForm =
    Html.form [ onSubmit Register ]
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Username"
            , onInput SetUsername
            ]
            []
        , Form.input
            [ class "form-control-lg"
            , placeholder "Email"
            , onInput SetEmail
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput SetPassword
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput SetPassword
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign up" ]
        ]
