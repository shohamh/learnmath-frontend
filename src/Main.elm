module Main exposing (..)

import Forms.Login
import Forms.Question
import Forms.Register
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Material
import Material.Layout as Layout


-- MODEL


type alias Model =
    { count : Int
    , selectedTab : Int
    , registerForm : Forms.Register.Model
    , loginForm : Forms.Login.Model
    , questionForm : Forms.Question.Model
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
    , questionForm = Forms.Question.model
    , mdl =
        Material.model

    -- Boilerplate: Always use this initial Mdl model store.
    }



-- ACTION, UPDATE


type Msg
    = SelectTab Int
    | RegisterFormHandler Forms.Register.Msg
    | LoginFormHandler Forms.Login.Msg
    | QuestionFormHandler Forms.Question.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab num ->
            { model | selectedTab = num } ! []

        RegisterFormHandler msg_ ->
            let
                ( newmodel, cmd ) =
                    Forms.Register.update msg_ model.registerForm
            in
            ( { model
                | registerForm = newmodel
              }
            , Cmd.map RegisterFormHandler cmd
            )

        LoginFormHandler msg_ ->
            let
                ( newmodel, cmd ) =
                    Forms.Login.update msg_ model.loginForm
            in
            ( { model
                | loginForm = newmodel
              }
            , Cmd.map LoginFormHandler cmd
            )

        QuestionFormHandler msg_ ->
            let
                ( newmodel, cmd ) =
                    Forms.Question.update msg_ model.questionForm
            in
            ( { model
                | questionForm = newmodel
              }
            , Cmd.map QuestionFormHandler cmd
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model


tabTitles : List (Html msg)
tabTitles =
    [ text "Register"
    , text "Login"
    , text "Question"
    ]


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "main.css"
            ]

        children =
            []
    in
    node tag attrs children


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
    div []
        [ {- stylesheet
             ,
          -}
          case model.selectedTab of
            0 ->
                Html.map RegisterFormHandler (Forms.Register.viewForm model.registerForm)

            1 ->
                Html.map LoginFormHandler (Forms.Login.viewForm model.loginForm)

            2 ->
                Html.map QuestionFormHandler (Forms.Question.viewForm model.questionForm)

            _ ->
                text "404"
        ]


subs : Model -> Sub Msg
subs model =
    Sub.batch [ Sub.map QuestionFormHandler (Forms.Question.subs model.questionForm), Layout.subs Mdl model.mdl ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { model | mdl = Layout.setTabsWidth 1384 model.mdl }
            , Layout.sub0 Mdl
            )
        , view = view
        , subscriptions = subs
        , update = update
        }
