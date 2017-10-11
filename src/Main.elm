module Main exposing (..)

import Page.Login
import Page.Question
import Page.Register
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Material
import Material.Layout as Layout
import Json.Decode as Decode exposing (Value)
import Route exposing (Route)
import Navigation exposing (Location)
import Task
import Views.Page as Page exposing (ActivePage)

-- MODEL


type alias Model =
    { pageState : PageState
    , session : Session
    , count : Int
    , selectedTab : Int
    , registerPage : Page.Register.Model
    , loginPage : Page.Login.Model
    , questionPage : Page.Question.Model
    , mdl :
        Material.Model

    -- Boilerplate: model store for any and all Mdl components you use.
    }


model : Model
model =
    { count = 0
    , selectedTab = 0
    , registerPage = Page.Register.model
    , loginPage = Page.Login.model
    , questionPage = Page.Question.model
    , mdl =
        Material.model

    -- Boilerplate: Always use this initial Mdl model store.
    }

initialPage : Page
initialPage =
    Blank


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)

-- ACTION, UPDATE


type Msg
    = SelectTab Int
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | QuestionMsg Question.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab num ->
            { model | selectedTab = num } ! []

        RegisterPageHandler msg_ ->
            let
                ( newmodel, cmd ) =
                    Page.Register.update msg_ model.registerPage
            in
            ( { model
                | registerPage = newmodel
              }
            , Cmd.map RegisterPageHandler cmd
            )

        LoginPageHandler msg_ ->
            let
                ( newmodel, cmd ) =
                    Page.Login.update msg_ model.loginPage
            in
            ( { model
                | loginPage = newmodel
              }
            , Cmd.map LoginPageHandler cmd
            )

        QuestionPageHandler msg_ ->
            let
                ( newmodel, cmd ) =
                    Page.Question.update msg_ model.questionPage
            in
            ( { model
                | questionPage = newmodel
              }
            , Cmd.map QuestionPageHandler cmd
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model

type Page
    = Blank
    | Home Home.Model
    | Login Login.Model
    | Register Register.Model
    | Question Question.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page

getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page
        TransitioningFrom page ->
            page

pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Question _ ->
            Sub.none

        Home _ ->
            Sub.none

        Login _ ->
            Sub.none

        Register _ ->
            Sub.none


tabTitles : List (Html msg)
tabTitles =
    [ text toString Register
    , text toString Login
    , text toString Question
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
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user
    in
    case page of
        Blank ->
            -- This is for the very initial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text "loading..." |> frame Page.Other
        Login subModel ->
            Login.view session subModel |> frame Page.Other |> Html.map LoginMsg

        Register subModel ->
            Register.view session subModel |> frame Page.Other |> Html.map RegisterMsg

        Question subModel ->
            Question.view session subModel |> frame Page.Other |> Html.map QuestionMsg


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
                Html.map RegisterPageHandler (Page.Register.viewPage model.registerPage)

            1 ->
                Html.map LoginPageHandler (Page.Login.viewPage model.loginPage)

            2 ->
                Html.map QuestionPageHandler (Page.Question.viewPage model.questionPage)

            _ ->
                text "404"
        ]


subs : Model -> Sub Msg
subs model =
    Sub.batch [ Sub.map QuestionPageHandler (Page.Question.subs model.questionPage), Layout.subs Mdl model.mdl ]

init : Value -> Location -> ( Model, Cmd Msg )
init value location =
    ( { model | mdl = Layout.setTabsWidth 1384 model.mdl }
            , Layout.sub0 Mdl
            )
main : Program Never Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , subscriptions = subs
        , update = update
        }
