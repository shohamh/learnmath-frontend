module Page.Question exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JDP exposing (decode, required)
import Json.Encode as JE exposing (..)
import Ports
import Util exposing ((=>), httpPost)


type alias Model =
    { successMessage : String
    , errorMessages : List String
    , lastExport : String
    , exportCount : Int
    , lastConvert : String
    , convertCount : Int
    , question : String
    , isCorrect : Maybe Bool
    }


model : Model
model =
    { successMessage = ""
    , errorMessages = []
    , lastExport = ""
    , exportCount = 0
    , lastConvert = ""
    , convertCount = 0
    , question = ""
    , isCorrect = Nothing
    }


type Msg
    = MyScriptExport String
    | MyScriptConvert String
    | CheckSolution
    | LoadQuestionResult (Result Http.Error LoadQuestionResponseData)
    | CheckSolutionResult (Result Http.Error CheckSolutionResponseData)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        MyScriptExport str ->
            { model | lastExport = Debug.log "latestExport" str, exportCount = model.exportCount + 1 }
                ! [ if model.exportCount <= 2 then
                        Ports.myscriptConvert ()
                    else
                        Cmd.none
                  ]

        MyScriptConvert str ->
            { model | lastConvert = Debug.log "latestConvert" str, convertCount = model.convertCount + 1 }
                ! []

        CheckSolution ->
            model ! [ checkSolution session model ]

        CheckSolutionResult (Err httpError) ->
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

        CheckSolutionResult (Ok resp) ->
            { model
                | isCorrect = Just resp.correct
                , errorMessages = List.append model.errorMessages resp.error_messages
            }
                => Cmd.none

        LoadQuestionResult (Err httpError) ->
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

        LoadQuestionResult (Ok resp) ->
            let
                newModel =
                    { model
                        | question = resp.problem
                    }
            in
            newModel => Ports.importQuestion (Just (Debug.log "question from server" newModel.question))


type alias Question =
    { mathml : String
    }


type alias Solution =
    { mathml : String
    }


type alias LoadQuestionResponseData =
    { success : Bool
    , error_messages : List String
    , problem : String
    }


type alias CheckSolutionResponseData =
    { success : Bool
    , error_messages : List String
    , correct : Bool
    }


loadQuestion : Session -> Model -> Cmd Msg
loadQuestion session model =
    httpPost "question" (questionFromModel model) questionEncoder loadQuestionResponseDecoder LoadQuestionResult


checkSolution : Session -> Model -> Cmd Msg
checkSolution session model =
    httpPost "check_solution" model solquesEncoder checkSolutionResponseDecoder CheckSolutionResult


questionFromModel : Model -> Question
questionFromModel model =
    Question model.question


solutionFromModel : Model -> Solution
solutionFromModel model =
    Solution model.lastExport



-- temp


solquesEncoder : Model -> JE.Value
solquesEncoder model =
    JE.object [ ( "solutions", JE.string model.lastExport ), ( "question", JE.string model.question ) ]


solutionEncoder : Solution -> JE.Value
solutionEncoder solution =
    JE.object
        [ ( "solution", JE.string solution.mathml )
        ]


questionEncoder : Question -> JE.Value
questionEncoder question =
    JE.object
        [ ( "mathml", JE.string question.mathml )
        ]


loadQuestionResponseDecoder : Decoder LoadQuestionResponseData
loadQuestionResponseDecoder =
    JDP.decode LoadQuestionResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "problem" JD.string


checkSolutionResponseDecoder : Decoder CheckSolutionResponseData
checkSolutionResponseDecoder =
    JDP.decode CheckSolutionResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "correct" JD.bool


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
                [ div [ class "col-md-12" ]
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
                        , onConvert MyScriptConvert
                        , attribute "applicationkey" "22bd37fa-2ee4-4bfd-98d9-137a39b81720"
                        , attribute "hmackey" "b79d64ad-89ba-4eed-a302-dee159005446"
                        ]
                        []
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ viewErrorMessages model.errorMessages
                    , case model.isCorrect of
                        Nothing ->
                            div [] []

                        Just correct ->
                            div []
                                [ case correct of
                                    True ->
                                        text "Correct! Good job!"

                                    False ->
                                        text "Incorrect, check your work for mistakes and try again!"
                                ]
                    , button [ class "btn btn-lg btn-primary pull-xs-right", onClick CheckSolution ]
                        [ text "Check Solution" ]
                    ]
                ]
            ]
        ]


onConvert : (String -> msg) -> Attribute msg
onConvert message =
    on "convert" (JD.map message (JD.at [ "target", "__data", "exports", "application/mathml+xml" ] JD.string))


onExport : (String -> msg) -> Attribute msg
onExport message =
    on "exports-changed" (JD.map message (JD.at [ "detail", "value", "application/mathml+xml" ] JD.string))
