module Page.Home exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.User as User
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Route
import Task exposing (Task)
import Views.Page as Page


type Msg
    = NoOp


type alias Model =
    { a : Int }


view : Session -> Model -> Html Msg
view session model =
    div [ class "home-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-3" ]
                    [ div [ class "sidebar" ]
                        [ case session.user of
                            Just user ->
                                text <| "Hi " ++ User.usernameToString user.username

                            Nothing ->
                                div []
                                    [ text "Hi random person! Would be great if you "
                                    , a [ Route.href Route.Login ] [ text "logged in!" ]
                                    , br [] []
                                    , text "Don't have an account? "
                                    , a [ Route.href Route.Register ] [ text "Sign up!" ]
                                    ]
                        ]
                    ]
                ]
            ]
        ]


init : Session -> Task PageLoadError Model
init session =
    let
        handleLoadError _ =
            pageLoadError Page.Home "Homepage is currently unavailable."
    in
    Task.succeed (Model 3)
        |> Task.mapError handleLoadError



{- let
       feedSources =
           if session.user == Nothing then
               SelectList.singleton globalFeed
           else
               SelectList.fromLists [] yourFeed [ globalFeed ]

       loadTags =
           Request.Article.tags
               |> Http.toTask

       loadSources =
           Feed.init session feedSources

       handleLoadError _ =
           pageLoadError Page.Home "Homepage is currently unavailable."
   in
   Task.map2 Model loadTags loadSources
       |> Task.mapError handleLoadError
-}


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            model ! []
