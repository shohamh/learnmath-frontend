module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Layout as Layout
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css, when, onClick, onInput)
import Material.Textfield as Textfield
import Json.Decode
import Json.Encode


-- MODEL


type alias Model =
    { count : Int
    , selectedTab : Int
    , register_password : String
    , register_passwordAgain : String
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


model : Model
model =
    { count = 0
    , selectedTab = 0
    , register_password = ""
    , register_passwordAgain = ""
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }



-- ACTION, UPDATE


type Msg
    = SelectTab Int
    | Update_register_password String
    | Update_register_passwordAgain String
    | SubmitRegisterForm
    | SubmitLoginForm
    | Mdl (Material.Msg Msg)



-- Boilerplate: Msg clause for internal Mdl messages.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab num ->
            { model | selectedTab = num } ! []

        Update_register_password str ->
            { model | register_password = str } ! []

        Update_register_passwordAgain str ->
            { model | register_passwordAgain = str } ! []

        SubmitRegisterForm ->
            model
                ! [ sendRegister
                        { username = model.register_username
                        , password = model.register_password
                        , email = model.register_email
                        }
                  ]

        RegistrationResult (Ok successMessage) ->
            { model | registration_result = successMessage } ! []

        RegistrationResult (Err _) ->
            model ! []

        SubmitLoginForm ->
            model ! []

        LoginResult (Ok successMessage) ->
            { model | login_result = successMessage } ! []

        LoginResult (Err _) ->
            model ! []

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


type alias RegisterData =
    { username : String
    , password : String
    , email : String
    }


sendRegister : RegisterData -> Cmd Msg
sendRegister registerData =
    let
        url =
            "http://learnmath.pythonanywhere.com/register"
                jsonBody
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


decodeRegisterResult : Json.Decoder JsonResult
decodeRegisterResult =
    map2 JsonResult
        (Json.Decode.field "success" Bool)
        (Json.Decode.field
            "error_message"
            String
        )



-- VIEW


type alias Mdl =
    Material.Model


tabTitles =
    [ text "Register"
    , text "Login"
    ]


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.onSelectTab SelectTab
        , Layout.selectedTab model.selectedTab
        ]
        { header =
            [ text "LearnMath"
            ]
        , drawer = [ text "drawer text" ]
        , tabs = ( tabTitles, [] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            registerForm model

        1 ->
            loginForm model

        _ ->
            text "404"


registerForm : Model -> Html Msg
registerForm model =
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
            , Options.onInput Update_register_password
            , Textfield.error ("Passwords don't match.") |> Options.when (model.register_password /= model.register_passwordAgain)
            ]
            []
        , Textfield.render Mdl
            [ 2 ]
            model.mdl
            [ Textfield.label "Confirm password"
            , Textfield.floatingLabel
            , Textfield.password
            , Options.onInput Update_register_passwordAgain
            , Textfield.error ("Passwords don't match.") |> Options.when (model.register_password /= model.register_passwordAgain)
            ]
            []
        , Textfield.render Mdl
            [ 3 ]
            model.mdl
            [ Textfield.label "Email"
            , Textfield.floatingLabel
            , Textfield.email
            ]
            []
        , Button.render Mdl
            [ 4 ]
            model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Options.onClick SubmitRegisterForm
            ]
            [ text "Register" ]
        ]


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
            , Options.onClick SubmitLoginForm
            ]
            [ text "Login" ]
        ]



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { model | mdl = Layout.setTabsWidth 1384 model.mdl }
            , Layout.sub0 Mdl
            )
        , view = view
        , subscriptions = .mdl >> Layout.subs Mdl
        , update = update
        }
