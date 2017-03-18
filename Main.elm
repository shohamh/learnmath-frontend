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
import Forms.Register
import Forms.Login


-- MODEL


type alias Model =
    { count : Int
    , selectedTab : Int
    , registerForm : Forms.Register.Model
    , loginForm : Forms.Login.Model
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


model : Model
model =
    { count = 0
    , selectedTab = 0
    , registerForm = Forms.Register.model
    , loginForm = Forms.Login.model
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }



-- ACTION, UPDATE


type Msg
    = SelectTab Int
    | RegisterFormHandler Forms.Register.Msg
    | LoginFormHandler Forms.Login.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab num ->
            { model | selectedTab = num } ! []

        RegisterFormHandler msg ->
            let
                ( newmodel, cmd ) =
                    Forms.Register.update msg model.registerForm
            in
                ( { model
                    | registerForm = newmodel
                  }
                , Cmd.map RegisterFormHandler cmd
                )

        LoginFormHandler msg ->
            let
                ( newmodel, cmd ) =
                    Forms.Login.update msg model.loginForm
            in
                ( { model
                    | loginForm = newmodel
                  }
                , Cmd.map LoginFormHandler cmd
                )

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
            Html.map RegisterFormHandler
                (Forms.Register.viewForm
                    model.registerForm
                )

        1 ->
            Html.map LoginFormHandler
                (Forms.Login.viewForm
                    model.loginForm
                )

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
