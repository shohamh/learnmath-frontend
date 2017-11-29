module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Data.User as User exposing (Username)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | Login
    | Logout
    | Register
    | Question
    | AddQuestion
    | Dashboard Username
    | TeacherDashboard



-- | Profile Username


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Register (s "register")
        , Url.map Question (s "question")
        , Url.map AddQuestion (s "add_question")
        , Url.map Dashboard (s "dashboard" </> User.usernameParser)
        , Url.map TeacherDashboard (s "teacher_dashboard")

        --       , Url.map Profile (s "profile" </> User.usernameParser)
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Question ->
                    [ "question" ]

                AddQuestion ->
                    [ "add_question" ]

                Dashboard username ->
                    [ "dashboard", User.usernameToString username ]

                TeacherDashboard ->
                    [ "teacher_dashboard" ]

        {- Profile username ->
           [ "profile", User.usernameToString username ]
        -}
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
