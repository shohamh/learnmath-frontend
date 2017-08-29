module Forms.Question exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (..)
import Material
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css, onClick, onInput, when)
import Material.Textfield as Textfield


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
    , error_messages : List String
    }


type Msg
    = UpdateUsername String
    | UpdatePassword String
    | Submit
    | SubmitResult (Result Http.Error ResponseData)
    | Mdl (Material.Msg Msg)


requestModel : Model -> RequestData
requestModel model =
    RequestData model.username model.password


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        UpdateUsername str ->
            { model | username = str } ! []

        UpdatePassword str ->
            { model | password = str } ! []

        Submit ->
            let
                requestData =
                    requestModel model
            in
            model ! [ send requestData ]

        SubmitResult (Ok successMessage) ->
            model ! []

        SubmitResult (Err errorMessage) ->
            model ! []


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
        |> required "error_messages" (JD.list JD.string)


type alias Mdl =
    Material.Model


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
        , Html.node "myscript-math-web" [ attribute "applicationkey" "22bd37fa-2ee4-4bfd-98d9-137a39b81720", attribute "hmackey" "b79d64ad-89ba-4eed-a302-dee159005446" ] []
        ]
