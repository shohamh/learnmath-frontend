module Page.Question exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (..)
import Route
import Views.Form as Form
import Views.Page as Page
import WebSocket


type alias Model =
    { successMessage : String
    , errorMessages : List String
    , lastExport : String
    }


model : Model
model =
    { successMessage = ""
    , errorMessages = []
    , lastExport = ""
    }


type Msg
    = MyScriptExport String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MyScriptExport str ->
            { model | lastExport = Debug.log "latestExport" str } ! []


subs : Model -> Sub Msg
subs model =
    Sub.none


view : Session -> Model -> Html Msg
view session model =
    div [ class "question-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ let
                        mimetypes =
                            [ "application/x-latex", "application/mathml+xml" ]

                        mimetypesEncoded =
                            JE.encode 0 (JE.list (List.map JE.string mimetypes))
                      in
                      Html.node "myscript-math-web"
                        [ attribute "mimetypes" mimetypesEncoded
                        , attribute "scheme" "http"
                        , attribute "host" "cloud.myscript.com"
                        , onExport MyScriptExport
                        , attribute "applicationkey" "22bd37fa-2ee4-4bfd-98d9-137a39b81720"
                        , attribute "hmackey" "b79d64ad-89ba-4eed-a302-dee159005446"
                        ]
                        []
                    ]
                ]
            ]
        ]


onExport : (String -> msg) -> Attribute msg
onExport message =
    on "exports-changed" (JD.map message (JD.at [ "detail", "value", "application/mathml+xml" ] JD.string))
