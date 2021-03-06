module Views.Page exposing (ActivePage(..), bodyId, frame)

-- The frame around a typical page - that is, the header and footer.

import Data.User as User exposing (Role(..), User, Username)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Util exposing ((=>))
import Views.Spinner exposing (spinner)


{- | Determines which navbar link (if any) will be rendered as active.
   Note that we don't enumerate every page here, because the navbar doesn't
   have links for every page. Anything that's not part of the navbar falls
   under Other.
-}


type ActivePage
    = Other
    | Home
    | Login
    | Register
    | Question
    | AddQuestion
    | AddPractice
    | Dashboard Username
    | TeacherDashboard



--| Profile Username


{-| Take a page's Html and frame it with a header and footer.
The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.
isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)
-}
frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "page-frame" ]
        [ viewHeader page user isLoading
        , content

        --, viewFooter
        ]


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "LearnMath" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                lazy2 Util.viewIf isLoading spinner
                    :: navbarLink (page == Home) Route.Home [ text "Home" ]
                    :: viewSignIn page user
            ]
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page maybeUser =
    case maybeUser of
        Nothing ->
            [ navbarLink (page == Login) Route.Login [ text "Sign in" ]
            , navbarLink (page == Register) Route.Register [ text "Sign up" ]
            ]

        Just user ->
            case user.role of
                Student ->
                    [ navbarLink (page == Dashboard user.username) (Route.Dashboard user.username) [ text "Dashboard" ]
                    , navbarLink (page == Question) Route.Question [ text "Practice" ]
                    , navbarLink False Route.Logout [ text "Sign out" ]
                    ]

                Teacher ->
                    [ navbarLink (page == TeacherDashboard) Route.TeacherDashboard [ text "Dashboard" ]
                    , navbarLink (page == AddQuestion) Route.AddQuestion [ text "Add Question" ]
                    , navbarLink (page == AddPractice) Route.AddPractice [ text "Add Practice" ]
                    , navbarLink False Route.Logout [ text "Sign out" ]
                    ]


viewFooter : Html msg
viewFooter =
    footer [ style [ ( "display", "inline" ) ] ]
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "LearnMath" ]
            , span [ class "attribution" ]
                [ text "A Netanya Academic College 2017 final year project."
                ]
            ]
        ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


{-| This id comes from index.html.
The Feed uses it to scroll to the top of the page (by ID) when switching pages
in the pagination sense.
-}
bodyId : String
bodyId =
    "page-body"
