module Page.Login exposing (ExternalMsg(..), Model, Msg(..), model, update, view)

import Config
import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User, Username(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (..)
import Ports
import Route
import Util exposing ((=>))
import Views.Form as Form


storeSession : User -> Cmd Msg
storeSession user =
    User.encode
        user
        |> JE.encode 0
        |> Just
        |> Ports.storeSession


type alias Model =
    { username : String
    , password : String
    , successMessage : String
    , errorMessages : List String
    }


model : Model
model =
    { username = ""
    , password = ""
    , successMessage = ""
    , errorMessages = []
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
    = SetUsername String
    | SetPassword String
    | Submit
    | SubmitResult (Result Http.Error ResponseData)


type ExternalMsg
    = NoOp
    | SetUser User


requestModel : Model -> RequestData
requestModel model =
    RequestData model.username model.password


dataToUser : Model -> ResponseData -> User
dataToUser model responseData =
    User "email@email.com" responseData.authToken (Username model.username)


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        SetUsername str ->
            { model | username = str } => Cmd.none => NoOp

        SetPassword str ->
            { model | password = str } => Cmd.none => NoOp

        Submit ->
            let
                requestData =
                    requestModel model
            in
            model => send requestData => NoOp

        SubmitResult (Ok responseData) ->
            if responseData.success then
                let
                    user =
                        dataToUser model responseData
                in
                model
                    => Cmd.batch [ storeSession user, Route.modifyUrl Route.Home ]
                    => SetUser user
            else
                { model
                    | errorMessages =
                        List.append model.errorMessages responseData.error_messages
                }
                    => Cmd.none
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
            Config.server ++ "/login"

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
    JDP.decode ResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "session_key" AuthToken.decoder
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
                    [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                    , p [ class "text-xs-center" ]
                        [ a [ Route.href Route.Register ]
                            [ text "Need an account?" ]
                        ]

                    --, Form.viewErrors model.errorMessages
                    , viewErrorMessages model.errorMessages
                    , viewForm
                    ]
                ]
            ]
        ]


viewForm : Html Msg
viewForm =
    Html.form [ onSubmit Submit ]
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Username"
            , onInput SetUsername
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput SetPassword
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign in" ]
        ]
