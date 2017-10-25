module Page.Question exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JDP exposing (decode, required)
import Json.Encode as JE exposing (..)
import Util exposing ((=>), httpPost)


type alias Model =
    { successMessage : String
    , errorMessages : List String
    , lastExport : String
    , question : String
    }


model : Model
model =
    { successMessage = ""
    , errorMessages = []
    , lastExport = ""
    , question = ""
    }


type Msg
    = MyScriptExport String
    | SolutionUpdateResult (Result Http.Error ResponseData)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        MyScriptExport str ->
            { model | lastExport = Debug.log "latestExport" str }
                ! [ httpPost "question" (requestModel model) requestEncoder responseDecoder SolutionUpdateResult
                  ]

        SolutionUpdateResult (Err httpError) ->
            let
                errorMessage =
                    case httpError of
                        Http.BadUrl str ->
                            "Bad url: " ++ str

                        Http.Timeout ->
                            "Request timed out."

                        Http.NetworkError ->
                            "Network error (no connectivity)."

                        Http.BadStatus response ->
                            "Bad status code returned: " ++ Basics.toString response.status.code

                        Http.BadPayload debug_str response ->
                            "JSON decoding of response failed: " ++ debug_str
            in
            { model
                | errorMessages =
                    List.append model.errorMessages
                        [ errorMessage
                        ]
            }
                => Cmd.none

        SolutionUpdateResult (Ok resp) ->
            { model | question = resp.problem } => Cmd.none


type alias RequestData =
    { mathml : String
    }


type alias ResponseData =
    { success : Bool
    , error_messages : List String
    , problem : String
    }


requestModel : Model -> RequestData
requestModel model =
    RequestData model.lastExport


requestEncoder : RequestData -> JE.Value
requestEncoder requestData =
    JE.object
        [ ( "mathml", JE.string requestData.mathml )
        ]


responseDecoder : Decoder ResponseData
responseDecoder =
    JDP.decode ResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "problem" JD.string


viewErrorMessages : List String -> Html Msg
viewErrorMessages errorMessages =
    div [] (List.intersperse (br [] []) (List.map text errorMessages))


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
                        , attribute "scheme" "https"
                        , attribute "host" "cloud.myscript.com"
                        , onExport MyScriptExport
                        , attribute "applicationkey" "22bd37fa-2ee4-4bfd-98d9-137a39b81720"
                        , attribute "hmackey" "b79d64ad-89ba-4eed-a302-dee159005446"
                        ]
                        []
                    , viewErrorMessages model.errorMessages
                    ]
                ]
            ]
        ]


onExport : (String -> msg) -> Attribute msg
onExport message =
    on "exports-changed" (JD.map message (JD.at [ "detail", "value", "application/mathml+xml" ] JD.string))
