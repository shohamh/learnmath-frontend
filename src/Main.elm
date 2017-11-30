module Main exposing (..)

import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.AddQuestion as AddQuestion
import Page.Dashboard as Dashboard
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Question as Question
import Page.Register as Register
import Page.TeacherDashboard as TeacherDashboard
import Ports
import Route exposing (Route)
import Task
import Util exposing ((=>))
import Views.Page as Page exposing (ActivePage)


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home Home.Model
    | Login Login.Model
    | Register Register.Model
    | Question Question.Model
    | AddQuestion AddQuestion.Model
    | Dashboard Dashboard.Model
    | TeacherDashboard TeacherDashboard.Model



--| Profile Username Profile.Model
-- MODEL


type alias Model =
    { pageState : PageState
    , session : Session
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        ( model_, cmd_ ) =
            setRoute (Route.fromLocation location)
                { pageState = Loaded initialPage
                , session = { user = decodeUserFromJson val }
                }
    in
    ( model_, cmd_ )


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)


initialPage : Page
initialPage =
    Blank



-- ACTION, UPDATE


type Msg
    = HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | QuestionMsg Question.Msg
    | AddQuestionMsg AddQuestion.Msg
    | DashboardMsg Dashboard.Msg
    | TeacherDashboardMsg TeacherDashboard.Msg
    | SetUser (Maybe User)
      --| ProfileMsg Profile.Msg
      --| ProfileLoaded Username (Result PageLoadError Profile.Model)
    | SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError Home.Model)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt toMsg task

        errored =
            pageErrored model
    in
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } => Cmd.none

        Just Route.Home ->
            transition HomeLoaded (Home.init model.session)

        Just Route.Question ->
            { model | pageState = Loaded (Question Question.model) } => Cmd.map QuestionMsg (Question.loadQuestion model.session Question.model)

        Just Route.AddQuestion ->
            { model | pageState = Loaded (AddQuestion AddQuestion.model) } => Cmd.map AddQuestionMsg (AddQuestion.loadCurriculumsAndSubjects model.session AddQuestion.model)

        Just Route.Login ->
            { model | pageState = Loaded (Login Login.model) } => Cmd.none

        Just Route.Logout ->
            let
                session =
                    model.session
            in
            { model | session = { session | user = Nothing } }
                => Cmd.batch
                    [ Ports.storeSession Nothing
                    , Route.modifyUrl Route.Home
                    ]

        Just Route.Register ->
            { model | pageState = Loaded (Register Register.model) } => Cmd.none

        Just (Route.Dashboard username) ->
            { model | pageState = Loaded (Dashboard Dashboard.model) } => Cmd.none

        Just Route.TeacherDashboard ->
            { model | pageState = Loaded (TeacherDashboard TeacherDashboard.model) } => Cmd.none



{- Just (Route.Profile username) ->
   transition (ProfileLoaded username) (Profile.init model.session username)
-}


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
    { model | pageState = Loaded (Errored error) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            setRoute route model

        ( HomeLoaded (Ok subModel), _ ) ->
            { model | pageState = Loaded (Home subModel) } => Cmd.none

        ( HomeLoaded (Err error), _ ) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        {- ( ProfileLoaded username (Ok subModel), _ ) ->
               { model | pageState = Loaded (Profile username subModel) } => Cmd.none

           ( ProfileLoaded username (Err error), _ ) ->
               { model | pageState = Loaded (Errored error) } => Cmd.none
        -}
        ( SetUser user, _ ) ->
            let
                session =
                    model.session

                cmd =
                    -- If we just signed out, then redirect to Home.
                    if session.user /= Nothing && user == Nothing then
                        Route.modifyUrl Route.Home
                    else
                        Cmd.none
            in
            { model | session = { session | user = user } }
                => cmd

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Login.update subMsg subModel

                newModel =
                    case msgFromPage of
                        Login.NoOp ->
                            model

                        Login.SetUser user ->
                            let
                                session =
                                    Debug.log "model.session"
                                        model.session
                            in
                            { model | session = { user = Just (Debug.log "after login - model.session.user" user) } }
            in
            { newModel | pageState = Loaded (Login pageModel) }
                => Cmd.map LoginMsg cmd

        ( RegisterMsg subMsg, Register subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Register.update subMsg subModel

                newModel =
                    case msgFromPage of
                        Register.NoOp ->
                            model

                        Register.SetUser user ->
                            let
                                session =
                                    model.session
                            in
                            { model | session = { user = Just (Debug.log "after register - model.session.user" user) } }
            in
            { newModel | pageState = Loaded (Register pageModel) }
                => Cmd.map RegisterMsg cmd

        ( HomeMsg subMsg, Home subModel ) ->
            toPage Home HomeMsg (Home.update session) subMsg subModel

        ( QuestionMsg subMsg, Question subModel ) ->
            toPage Question QuestionMsg (Question.update session) subMsg subModel

        ( AddQuestionMsg subMsg, AddQuestion subModel ) ->
            toPage AddQuestion AddQuestionMsg (AddQuestion.update session) subMsg subModel

        {- ( ProfileMsg subMsg, Profile username subModel ) ->
           toPage (Profile username) ProfileMsg (Profile.update model.session) subMsg subModel
        -}
        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model => Cmd.none

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model => Cmd.none



-- VIEW
--| Profile Profile.Model


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



{-
   tabTitles : List (Html msg)
   tabTitles =
       [ text toString Register
       , text toString Login
       , text toString Question
       ]
-}


stylesheet : Html Msg
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
        NotFound ->
            NotFound.view session |> frame Page.Other

        Blank ->
            -- This is for the very initial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text "loading..." |> frame Page.Other

        Errored subModel ->
            Errored.view session subModel |> frame Page.Other

        Home subModel ->
            Home.view session subModel |> frame Page.Home |> Html.map HomeMsg

        Login subModel ->
            Login.view session subModel |> frame Page.Other |> Html.map LoginMsg

        Register subModel ->
            Register.view session subModel |> frame Page.Other |> Html.map RegisterMsg

        Question subModel ->
            Question.view session subModel |> frame Page.Other |> Html.map QuestionMsg

        AddQuestion subModel ->
            AddQuestion.view session subModel |> frame Page.Other |> Html.map AddQuestionMsg

        Dashboard subModel ->
            Dashboard.view session subModel |> frame Page.Other |> Html.map DashboardMsg

        TeacherDashboard subModel ->
            TeacherDashboard.view session subModel |> frame Page.Other |> Html.map TeacherDashboardMsg


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        NotFound ->
            Sub.none

        Home _ ->
            Sub.none

        Login _ ->
            Sub.none

        Register _ ->
            Sub.none

        {- Profile _ _ ->
           Sub.none
        -}
        Question subModel ->
            Sub.map QuestionMsg (Question.subs subModel)

        AddQuestion subModel ->
            Sub.none

        Dashboard subModel ->
            Sub.none

        TeacherDashboard subModel ->
            Sub.none


sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)


subs : Model -> Sub Msg
subs model =
    Sub.batch [ pageSubscriptions (getPage model.pageState), Sub.map SetUser sessionChange ]


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , subscriptions = subs
        , update = update
        }
