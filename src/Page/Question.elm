module Page.Question exposing (..)

import Array exposing (Array)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JDP exposing (decode, required)
import Json.Encode as JE exposing (..)
import Ports
import Time exposing (..)
import Util exposing ((=>), httpPost, maybeJoin)


type alias Model =
    { successMessage : String
    , errorMessages : List String
    , currentQuestionIndex : Int
    , currentAnswers : Array String
    , solutionHistory : Array (List String)
    , exportCount : Int
    , lastConvert : String
    , convertCount : Int
    , questions : Array String
    , subjects : List String
    , curriculum : String
    , questionEditors : Array (Html Msg)
    , isCorrect : Array (Maybe Bool)
    , timers : Array Int
    , mistakeStep : Maybe Int
    , mistakeType : Maybe String
    }


model : Model
model =
    { successMessage = ""
    , errorMessages = []
    , currentAnswers = Array.empty
    , solutionHistory = Array.empty
    , currentQuestionIndex = 0
    , exportCount = 0
    , lastConvert = ""
    , convertCount = 0
    , questions = Array.empty
    , subjects = []
    , curriculum = ""
    , questionEditors = Array.empty
    , isCorrect = Array.empty
    , timers = Array.empty
    , mistakeStep = Nothing
    , mistakeType = Nothing
    }


type Msg
    = MyScriptExport String
    | MyScriptConvert ( String, String )
    | CheckSolution
    | Validate
    | PrevQuestion
    | NextQuestion
    | LoadQuestionResult (Result Http.Error LoadQuestionResponseData)
    | CheckSolutionResult (Result Http.Error CheckSolutionResponseData)
    | ValidateResult (Result Http.Error ValidateSolutionResponseData)
    | TimerTick Time


editor : Int -> Bool -> Html Msg
editor index show =
    let
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
        , id (Debug.log "id" ("myscript-editor-" ++ toString index))
        , style
            [ ( "display"
              , if show then
                    "block"
                else
                    "none"
              )
            ]
        ]
        []


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        PrevQuestion ->
            if model.currentQuestionIndex > 0 then
                { model
                    | currentQuestionIndex =
                        model.currentQuestionIndex - 1
                    , questionEditors = Array.set (model.currentQuestionIndex - 1) (editor (model.currentQuestionIndex - 1) True) <| Array.set model.currentQuestionIndex (editor model.currentQuestionIndex False) model.questionEditors
                }
                    ! []
            else
                model ! []

        NextQuestion ->
            if model.currentQuestionIndex + 1 < Array.length model.questions then
                { model
                    | currentQuestionIndex =
                        model.currentQuestionIndex + 1
                    , questionEditors = Array.set (model.currentQuestionIndex + 1) (editor (model.currentQuestionIndex - 1) True) <| Array.set model.currentQuestionIndex (editor model.currentQuestionIndex False) model.questionEditors
                }
                    ! []
            else
                model ! []

        MyScriptExport str ->
            { model
                | currentAnswers = Array.set model.currentQuestionIndex (Debug.log "latestExport" str) model.currentAnswers
                , exportCount = model.exportCount + 1
            }
                ! [ if model.exportCount <= 2 then
                        Ports.myscriptConvert ()
                    else
                        Cmd.none
                  ]

        MyScriptConvert ( mathml, stringIndex ) ->
            let
                ind =
                    Result.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 2 <| Array.fromList (String.split "-" stringIndex))))
            in
            { model
                | lastConvert = Debug.log "latestConvert" mathml
                , convertCount = model.convertCount + 1
                , solutionHistory = Array.set (Debug.log "ind" ind) (List.append (Maybe.withDefault [] <| Array.get model.currentQuestionIndex model.solutionHistory) [ mathml ]) model.solutionHistory
            }
                ! []

        Validate ->
            model ! [ validateSolution session model ]

        CheckSolution ->
            model ! [ checkSolution session model ]

        CheckSolutionResult (Err httpError) ->
            let
                errorMessage =
                    Util.httpErrorToString httpError
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
                | isCorrect = Array.set model.currentQuestionIndex (Just resp.correct) model.isCorrect
                , errorMessages = resp.error_messages
            }
                => Cmd.none

        ValidateResult (Err httpError) ->
            let
                errorMessage =
                    Util.httpErrorToString httpError
            in
            { model
                | errorMessages =
                    List.append model.errorMessages
                        [ errorMessage
                        ]
            }
                => Cmd.none

        ValidateResult (Ok resp) ->
            { model
                | isCorrect = Array.set model.currentQuestionIndex (Just resp.correct) model.isCorrect
                , mistakeStep = resp.step
                , mistakeType = resp.mistake_type
                , errorMessages = resp.error_messages
            }
                => Cmd.none

        LoadQuestionResult (Err httpError) ->
            let
                errorMessage =
                    Util.httpErrorToString httpError
            in
            { model
                | errorMessages =
                    List.append model.errorMessages [ errorMessage ]
            }
                => Cmd.none

        LoadQuestionResult (Ok resp) ->
            let
                stringList : List String
                stringList =
                    []

                newModel =
                    { model
                        | questions = Debug.log "questions haha" resp.questions
                        , currentAnswers = Array.repeat (Array.length resp.questions) ""
                        , isCorrect = Array.repeat (Array.length resp.questions) Nothing
                        , timers = Array.repeat (Array.length resp.questions) 0
                        , subjects = resp.subjects
                        , curriculum = resp.curriculum
                        , solutionHistory = Array.repeat (Array.length resp.questions) []
                        , questionEditors =
                            Array.indexedMap
                                (\index item ->
                                    editor index (model.currentQuestionIndex == index)
                                )
                                (Debug.log "resp.questions" resp.questions)
                    }
            in
            if resp.success then
                newModel
                    ! Array.toList
                        (Array.indexedMap
                            (\index item ->
                                Ports.importQuestion
                                    ( Debug.log "question from server" item, Debug.log "index" index )
                            )
                            newModel.questions
                        )
            else
                { model | errorMessages = resp.error_messages } ! []

        TimerTick time ->
            { model
                | timers =
                    Array.set model.currentQuestionIndex ((Maybe.withDefault 42 <| Array.get model.currentQuestionIndex model.timers) + 1) model.timers
            }
                ! []


type alias LoadQuestionResponseData =
    { success : Bool
    , error_messages : List String
    , questions : Array String
    , subjects : List String
    , curriculum : String
    }


type alias CheckSolutionResponseData =
    { success : Bool
    , error_messages : List String
    , correct : Bool
    }


type alias ValidateSolutionResponseData =
    { success : Bool
    , error_messages : List String
    , correct : Bool
    , step : Maybe Int
    , mistake_type : Maybe String
    }


loadQuestions : Session -> Model -> Cmd Msg
loadQuestions session model =
    httpPost "get_practice_session_questions" (Session.getSid session) loadQuestionsEncoder loadQuestionResponseDecoder LoadQuestionResult


loadQuestionsEncoder : String -> JE.Value
loadQuestionsEncoder sid =
    JE.object
        [ ( "sid", JE.string sid )
        ]


checkSolutionEncoder : ( Session, Model ) -> JE.Value
checkSolutionEncoder ( session, model ) =
    JE.object
        [ ( "sid", JE.string (Session.getSid session) )
        , ( "solutions"
          , case Array.get model.currentQuestionIndex model.currentAnswers of
                Just answer ->
                    JE.string answer

                Nothing ->
                    JE.null
          )
        , ( "index", JE.int model.currentQuestionIndex )
        , ( "time"
          , case Array.get model.currentQuestionIndex model.timers of
                Just time ->
                    JE.int time

                Nothing ->
                    JE.null
          )
        ]


validateSolutionEncoder : ( Session, Model ) -> JE.Value
validateSolutionEncoder ( session, model ) =
    JE.object
        [ ( "sid", JE.string (Session.getSid session) )
        , ( "solution_history"
          , JE.list
                (List.map JE.string (Maybe.withDefault [] <| Array.get model.currentQuestionIndex model.solutionHistory))
          )
        , ( "index", JE.int model.currentQuestionIndex )
        , ( "time"
          , case Array.get model.currentQuestionIndex model.timers of
                Just time ->
                    JE.int time

                Nothing ->
                    JE.null
          )
        ]


checkSolution : Session -> Model -> Cmd Msg
checkSolution session model =
    httpPost "check_solution" ( session, model ) checkSolutionEncoder checkSolutionResponseDecoder CheckSolutionResult


validateSolution : Session -> Model -> Cmd Msg
validateSolution session model =
    httpPost "validate_solution" ( session, model ) validateSolutionEncoder validateSolutionResponseDecoder ValidateResult


loadQuestionResponseDecoder : Decoder LoadQuestionResponseData
loadQuestionResponseDecoder =
    JDP.decode LoadQuestionResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "questions" (JD.array JD.string)
        |> JDP.required "subjects" (JD.list JD.string)
        |> JDP.required "curriculum" JD.string


checkSolutionResponseDecoder : Decoder CheckSolutionResponseData
checkSolutionResponseDecoder =
    JDP.decode CheckSolutionResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "correct" JD.bool


validateSolutionResponseDecoder : Decoder ValidateSolutionResponseData
validateSolutionResponseDecoder =
    JDP.decode ValidateSolutionResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "correct" JD.bool
        |> JDP.optional "step"
            (JD.map
                (\x ->
                    if x == -1 then
                        Nothing
                    else
                        Just x
                )
                JD.int
            )
            (Just -1)
        |> JDP.optional "mistake_type"
            (JD.map
                (\x ->
                    if x == "" then
                        Nothing
                    else
                        Just x
                )
                JD.string
            )
            (Just "")


viewErrorMessages : List String -> Html Msg
viewErrorMessages errorMessages =
    div [] (List.intersperse (br [] []) (List.map text errorMessages))


subs : Model -> Sub Msg
subs model =
    every second TimerTick


view : Session -> Model -> Html Msg
view session model =
    div [ class "question-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ text "Practice subjects:"
                , ul [] (List.map (\x -> li [] [ text x ]) model.subjects)
                , br [] []
                , div []
                    [ case Array.length model.questions of
                        0 ->
                            text "Couldn't load questions."

                        _ ->
                            text ("Question " ++ toString (model.currentQuestionIndex + 1) ++ "/" ++ toString (Array.length model.questions))
                    ]
                , div []
                    [ button
                        [ class "btn btn-lg btn-primary pull-xs-right"
                        , onClick NextQuestion
                        , disabled <| not <| (model.currentQuestionIndex + 1 < Array.length model.questions)
                        ]
                        [ text "Next" ]
                    , button
                        [ class "btn btn-lg btn-primary pull-xs-right"
                        , onClick PrevQuestion
                        , disabled <| not <| (model.currentQuestionIndex > 0)
                        ]
                        [ text "Prev" ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    (Array.toList model.questionEditors)
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ viewErrorMessages model.errorMessages
                    , case maybeJoin (Array.get model.currentQuestionIndex model.isCorrect) of
                        Nothing ->
                            div [] []

                        Just correct ->
                            div []
                                [ if correct then
                                    text "Correct! Good job!"
                                  else
                                    text "Incorrect, check your work for mistakes and try again!"
                                ]
                    , button [ class "btn btn-lg btn-primary pull-xs-right", onClick Validate ]
                        [ text "Validate"
                        ]
                    , button [ class "btn btn-lg btn-primary pull-xs-right", onClick CheckSolution ]
                        [ text
                            ("Done "
                                ++ (case Array.get model.currentQuestionIndex model.timers of
                                        Just seconds ->
                                            toString (seconds // 60) ++ ":" ++ toString (seconds % 60)

                                        Nothing ->
                                            ""
                                   )
                            )
                        ]
                    ]
                ]
            ]
        ]


onConvert : (( String, String ) -> msg) -> Attribute msg
onConvert message =
    let
        map2d string int =
            message ( string, int )
    in
    on "convert" (JD.map2 map2d (JD.at [ "target", "__data", "exports", "application/mathml+xml" ] JD.string) (JD.at [ "" ] JD.string))


onExport : (String -> msg) -> Attribute msg
onExport message =
    on "exports-changed" (JD.map message (JD.at [ "detail", "value", "application/mathml+xml" ] JD.string))
