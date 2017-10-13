module Page.Login exposing (ExternalMsg(..), Model, Msg, model, update, view)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Html exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (..)
import Material
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css, onClick, onInput, when)
import Material.Textfield as Textfield
import Ports
import Route
import Util exposing ((=>))


storeSession : User -> Cmd Msg
storeSession user =
    User.encode user
        |> JE.encode 0
        |> Just
        |> Ports.storeSession


type alias Model =
    { username : String
    , password : String
    , successMessage : String
    , errorMessages : List String
    , mdl : Material.Model
    }


model : Model
model =
    { username = ""
    , password = ""
    , successMessage = ""
    , errorMessages = []
    , mdl = Material.model
    }


type alias RequestData =
    { username : String
    , password : String
    }


type alias ResponseData =
    { success : Bool
    , authToken : AuthToken
    , error_messages : List String
    }


type Msg
    = UpdateUsername String
    | UpdatePassword String
    | Submit
    | SubmitResult (Result Http.Error ResponseData)
    | Mdl (Material.Msg Msg)


type ExternalMsg
    = NoOp
    | SetUser User


requestModel : Model -> RequestData
requestModel model =
    RequestData model.username model.password


responseDataToUser : ResponseData -> Model -> User
responseDataToUser responseData model =
    User ("email@email.com" responseData.authToken (User.Username model.username))


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model => NoOp

        UpdateUsername str ->
            { model | username = str } => Cmd.none => NoOp

        UpdatePassword str ->
            { model | password = str } => Cmd.none => NoOp

        Submit ->
            let
                requestData =
                    requestModel model
            in
            model => send requestData => NoOp

        SubmitResult (Ok responseData) ->
            model
                => Cmd.batch [ storeSession (responseDataToUser responseData model), Route.modifyUrl Route.Home ]
                => NoOp

        SubmitResult (Err httpError) ->
            let
                errorMessage =
                    case httpError of
                        Http.BadUrl str ->
                            "Bad url: " ++ str

                        Http.Timeout ->
                            "Request timed out."

                        Http.NetworkError ->
                            "Network error (no connectivity on your side)."

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


send : RequestData -> Cmd Msg
send requestData =
    let
        url =
            "http://learnmath.pythonanywhere.com/login"

        body =
            Http.jsonBody <| requestEncoder requestData

        request =
            Http.post url body responseDecoder
    in
    Http.send SubmitResult request


requestEncoder : RequestData -> JE.Value
requestEncoder requestData =
    JE.object
        [ ( "username", JE.string requestData.username )
        , ( "password", JE.string requestData.password )
        ]


responseDecoder : Decoder ResponseData
responseDecoder =
    decode ResponseData
        |> required "success" JD.bool
        |> required "session_key" AuthToken.decoder
        |> required "error_messages" (JD.list JD.string)


type alias Mdl =
    Material.Model


viewErrorMessages : List String -> Html Msg
viewErrorMessages errorMessages =
    div [] (List.intersperse (br [] []) (List.map text errorMessages))


view : Session -> Model -> Html Msg
view session model =
    div []
        [ Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.label "Username"
            , Textfield.floatingLabel
            , Textfield.text_
            , Options.onInput UpdateUsername
            ]
            []
        , Textfield.render Mdl
            [ 1 ]
            model.mdl
            [ Textfield.label "Password"
            , Textfield.floatingLabel
            , Textfield.password
            , Options.onInput UpdatePassword
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
        , viewErrorMessages model.errorMessages
        , text model.successMessage
        ]
