module Page.Register exposing (..)

import Html exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (..)
import List
import Material
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css, onClick, onInput, when)
import Material.Textfield as Textfield


type alias Model =
    { username : String
    , password : String
    , passwordAgain : String
    , email : String
    , successMessage : String
    , errorMessages : List String
    , mdl : Material.Model
    }


model : Model
model =
    { username = ""
    , password = ""
    , passwordAgain = ""
    , email = ""
    , successMessage = ""
    , errorMessages = []
    , mdl = Material.model
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
    = UpdateUsername String
    | UpdatePassword String
    | UpdatePasswordAgain String
    | UpdateEmail String
    | Submit
    | SubmitResult (Result Http.Error ResponseData)
    | Mdl (Material.Msg Msg)


requestModel : Model -> RequestData
requestModel model =
    RequestData model.username model.password model.email


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        UpdateUsername str ->
            { model | username = str } ! []

        UpdatePassword str ->
            { model | password = str } ! []

        UpdatePasswordAgain str ->
            { model | passwordAgain = str } ! []

        UpdateEmail str ->
            { model | email = str } ! []

        Submit ->
            let
                requestData =
                    requestModel model
            in
            model ! [ send requestData ]

        SubmitResult (Ok responseData) ->
            { model
                | successMessage =
                    case responseData.success of
                        True ->
                            "Registration was successful."

                        False ->
                            "Registration failed."
                , errorMessages = responseData.errorMessages
            }
                ! []

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
                ! []


send : RequestData -> Cmd Msg
send requestData =
    let
        url =
            "http://learnmath.pythonanywhere.com/register"

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
        , ( "email", JE.string requestData.email )
        ]


responseDecoder : Decoder ResponseData
responseDecoder =
    decode ResponseData
        |> required "success" JD.bool
        |> required "error_messages" (JD.list JD.string)


type alias Mdl =
    Material.Model


viewErrorMessages : List String -> Html Msg
viewErrorMessages errorMessages =
    div [] (List.intersperse (br [] []) (List.map text errorMessages))


viewForm : Model -> Html Msg
viewForm model =
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
            , Textfield.error "Passwords don't match." |> Options.when (model.password /= model.passwordAgain)
            ]
            []
        , Textfield.render Mdl
            [ 2 ]
            model.mdl
            [ Textfield.label "Confirm password"
            , Textfield.floatingLabel
            , Textfield.password
            , Options.onInput UpdatePasswordAgain
            , Textfield.error "Passwords don't match." |> Options.when (model.password /= model.passwordAgain)
            ]
            []
        , Textfield.render Mdl
            [ 3 ]
            model.mdl
            [ Textfield.label "Email"
            , Textfield.floatingLabel
            , Textfield.email
            , Options.onInput UpdateEmail
            ]
            []
        , Button.render Mdl
            [ 4 ]
            model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Options.onClick Submit
            ]
            [ text "Register" ]
        , viewErrorMessages model.errorMessages
        , text model.successMessage
        ]
