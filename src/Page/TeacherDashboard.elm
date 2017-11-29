module Page.TeacherDashboard exposing (Model, Msg, model, update, view)

import Data.Session as Session exposing (Session)
import Data.User as User
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Route
import Views.Page as Page


type Msg
    = NoOp


type alias Model =
    {}


model : Model
model =
    {}


view : Session -> Model -> Html Msg
view session model =
    div [ class "feedback-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-12" ]
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


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            model ! []
