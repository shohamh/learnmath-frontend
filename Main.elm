module Main exposing (..)

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
      Material.Model
      -- Boilerplate: Always use this initial Mdl model store.
  }


-- ACTION, UPDATE


type Msg
    = SelectTab Int
    | Forms.Register.Msg
    | Forms.Login.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab num ->
            { model | selectedTab = num } ! []

        RegistrationResult (Ok successMessage) ->
            { model | registration_result = successMessage } ! []

        RegistrationResult (Err _) ->
            model ! []

        LoginResult (Ok successMessage) ->
            { model | login_result = successMessage } ! []

        LoginResult (Err _) ->
            model ! []

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


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
