module Views.Page exposing (ActivePage(..), Msg(..), bodyId, frame)

-- The frame around a typical page - that is, the header and footer.

import Data.User as User exposing (User, Username)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Material
import Material.Layout as Layout
import Page.Login
import Route exposing (Route)
import Util exposing ((=>))
import Views.Spinner exposing (spinner)


type alias Model =
    { mdl : Material.Model
    , selectedTab : Int
    , loginModel : Page.Login.Model
    }


model : Model
model =
    { mdl = Material.model
    , selectedTab = 0
    }


type Msg loginMsg
    = SelectTab Int
    | LiftMsg loginMsg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tabIndex ->
            { model | selectedTab = tabIndex } ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model

        LiftMsg msg_ ->
            let
                ( newLoginModel, newLoginCmd ) =
                    Page.Login.update msg_ model.loginModel
            in
            { model | loginModel = newLoginModel } ! [ newLoginCmd ]



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



--| Profile Username


tabList : List String
tabList =
    [ "Home"
    , "Login"
    , "Register"
    , "Question"
    ]


tabTitles : List (Html msg)
tabTitles =
    List.map text tabList


{-| Take a page's Html and frame it with a header and footer.
The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.
isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)
-}
frame : Bool -> Maybe User -> ActivePage -> Html Msg -> Html Msg
frame isLoading user page content =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.onSelectTab SelectTab
        , Layout.selectedTab model.selectedTab
        ]
        { header = [ viewHeader page user isLoading ]
        , drawer = [ text "drawer text" ]
        , tabs = ( tabTitles, [] )
        , main = [ Html.map LiftMsg content ]
        }



{- div [ class "page-frame" ]
   [ viewHeader page user isLoading
   , content
   , viewFooter
   ]
-}


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                lazy2 Util.viewIf isLoading spinner
                    :: navbarLink (page == Home) Route.Home [ text "Home" ]
                    :: viewSignIn page user
            ]
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [ navbarLink (page == Login) Route.Login [ text "Sign in" ]
            , navbarLink (page == Register) Route.Register [ text "Sign up" ]
            ]

        Just user ->
            [ {- navbarLink

                  (page == Profile user.username)
                    (Route.Profile user.username)
                    [ {- img [ class "user-pic", {- UserPhoto.src -} "https://i.imgur.com/RBeajsL.jpg" user.image ] []
                         ,
                      -}
                      User.usernameToHtml user.username
                    ]

                 ,
              -}
              navbarLink False Route.Logout [ text "Sign out" ]
            ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
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
