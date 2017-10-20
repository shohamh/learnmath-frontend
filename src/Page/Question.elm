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
    }


model : Model
model =
    { successMessage = ""
    , errorMessages = []
    }


type Msg
    = MyscriptReceive String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MyscriptReceive str ->
            model ! [ Debug.log str WebSocket.send "ws://echo.websocket.org" ("Hello" ++ str) ]


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ WebSocket.listen "wss://cloud.myscript.com/api/v3.0/recognition/ws/math" MyscriptReceive

        --, WebSocket.listen "wss://cloud.myscript.com" MyscriptReceive
        ]



--WebSocket.listen "wss://cloud.myscript.com" MyscriptReceive


view : Session -> Model -> Html Msg
view session model =
    div [ class "question-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ Html.node "myscript-math-web" [ attribute "applicationkey" "22bd37fa-2ee4-4bfd-98d9-137a39b81720", attribute "hmackey" "b79d64ad-89ba-4eed-a302-dee159005446" ] []
                    ]
                ]
            ]
        ]
