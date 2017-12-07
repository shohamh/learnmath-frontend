module Page.Dashboard exposing (Model, Msg, loadMistakeTypes, loadStudentPerformance, loadSubjectPerformance, model, update, view)

import Data.Session as Session exposing (Session)
import Data.Subject exposing (Subject(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JDP exposing (decode, required)
import Json.Encode as JE exposing (..)
import SampleData exposing (studentPerformanceInClass, subjectPerformance)
import Util exposing (httpPost)
import Visualizations.BarChart as BarChart exposing (viewBarChart)
import Visualizations.LineChart as LineChart exposing (viewLineChart)
import Visualizations.PieChart as PieChart exposing (viewPieChart)


type alias Model =
    { barChartModel : BarChart.Model
    , lineChartModel : LineChart.Model
    , pieChartModel : PieChart.Model
    , errorMessages : List String
    }


model : Model
model =
    { barChartModel = BarChart.model
    , lineChartModel = LineChart.model
    , pieChartModel = PieChart.model
    , errorMessages = []
    }


type alias LoadSubjectPerformanceResponseData =
    { success : Bool
    , error_messages : List String
    , result : List ( Subject, ( Float, Float ) )
    }


type alias LoadStudentPerformanceResponseData =
    { success : Bool
    , error_messages : List String
    , result : List ( String, ( Float, Float ) )
    }


type alias LoadMistakeTypesResponseData =
    { success : Bool
    , error_messages : List String
    , result : List ( String, Float )
    }


type Msg
    = LoadStudentPerformance (Result Http.Error LoadStudentPerformanceResponseData)
    | LoadSubjectPerformance (Result Http.Error LoadSubjectPerformanceResponseData)
    | LoadMistakeTypes (Result Http.Error LoadMistakeTypesResponseData)


loadStudentPerformance : Session -> Model -> Cmd Msg
loadStudentPerformance session model =
    httpPost "get_success_percentage_avg_time_stats" ( session, model ) loadStudentPerformanceEncoder loadStudentPerformanceResponseDecoder LoadStudentPerformance


loadStudentPerformanceEncoder : ( Session, Model ) -> JE.Value
loadStudentPerformanceEncoder ( session, model ) =
    JE.object
        [ ( "sid", JE.string (Session.getSid session) )
        ]


studentPerformanceTransformer : Dict String (Dict String Float) -> List ( String, ( Float, Float ) )
studentPerformanceTransformer dict =
    List.map
        (\studentName ->
            ( studentName
            , ( Maybe.withDefault 0.5 (Dict.get "success_percentage" (Maybe.withDefault Dict.empty (Dict.get studentName dict)))
              , Maybe.withDefault 100 (Dict.get "average_solving_time" (Maybe.withDefault Dict.empty (Dict.get studentName dict)))
              )
            )
        )
        (Dict.keys dict)


loadStudentPerformanceResponseDecoder : Decoder LoadStudentPerformanceResponseData
loadStudentPerformanceResponseDecoder =
    JDP.decode LoadStudentPerformanceResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "result" (JD.map studentPerformanceTransformer (JD.dict (JD.dict JD.float)))


loadSubjectPerformance : Session -> Model -> Cmd Msg
loadSubjectPerformance session model =
    httpPost "get_wrong_right_stats" ( session, model ) loadSubjectPerformanceEncoder loadSubjectPerformanceResponseDecoder LoadSubjectPerformance


loadSubjectPerformanceEncoder : ( Session, Model ) -> JE.Value
loadSubjectPerformanceEncoder ( session, model ) =
    JE.object
        [ ( "sid", JE.string (Session.getSid session) )
        ]


subjectPerformanceTransformer : Dict String (Dict String Float) -> List ( Subject, ( Float, Float ) )
subjectPerformanceTransformer dict =
    List.map
        (\subjectName ->
            ( Subject subjectName
            , ( Maybe.withDefault 0 (Dict.get "wrong" (Maybe.withDefault Dict.empty (Dict.get subjectName dict)))
              , Maybe.withDefault 0 (Dict.get "correct" (Maybe.withDefault Dict.empty (Dict.get subjectName dict)))
              )
            )
        )
        (Dict.keys dict)


loadSubjectPerformanceResponseDecoder : Decoder LoadSubjectPerformanceResponseData
loadSubjectPerformanceResponseDecoder =
    JDP.decode LoadSubjectPerformanceResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "result" (JD.map subjectPerformanceTransformer (JD.dict (JD.dict JD.float)))


loadMistakeTypes : Session -> Model -> Cmd Msg
loadMistakeTypes session model =
    httpPost "get_mistake_type_stats" ( session, model ) loadMistakeTypesEncoder loadMistakeTypesResponseDecoder LoadMistakeTypes


loadMistakeTypesEncoder : ( Session, Model ) -> JE.Value
loadMistakeTypesEncoder ( session, model ) =
    JE.object
        [ ( "sid", JE.string (Session.getSid session) )
        ]


mistakeTypesTransformer : Dict String (Dict String Float) -> List ( String, Float )
mistakeTypesTransformer dict =
    List.map
        (\mistakeType ->
            ( mistakeType
            , Maybe.withDefault 0 (Dict.get mistakeType (Maybe.withDefault Dict.empty (Dict.get mistakeType dict)))
            )
        )
        (Dict.keys dict)


loadMistakeTypesResponseDecoder : Decoder LoadMistakeTypesResponseData
loadMistakeTypesResponseDecoder =
    JDP.decode LoadMistakeTypesResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)
        |> JDP.required "result" (JD.map mistakeTypesTransformer (JD.dict (JD.dict JD.float)))


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        LoadStudentPerformance (Ok resp) ->
            { model | lineChartModel = resp.result } ! []

        LoadStudentPerformance (Err err) ->
            let
                errorMessage =
                    Util.httpErrorToString err
            in
            { model
                | errorMessages =
                    List.append model.errorMessages
                        [ errorMessage
                        ]
            }
                ! []

        LoadSubjectPerformance (Ok resp) ->
            { model | barChartModel = resp.result } ! []

        LoadSubjectPerformance (Err err) ->
            let
                errorMessage =
                    Util.httpErrorToString err
            in
            { model
                | errorMessages =
                    List.append model.errorMessages
                        [ errorMessage
                        ]
            }
                ! []

        LoadMistakeTypes (Ok resp) ->
            { model | pieChartModel = resp.result } ! []

        LoadMistakeTypes (Err err) ->
            let
                errorMessage =
                    Util.httpErrorToString err
            in
            { model
                | errorMessages =
                    List.append model.errorMessages
                        [ errorMessage
                        ]
            }
                ! []


view : Session -> Model -> Html Msg
view session model =
    div [ class "dashboard-page" ]
        [ div [ class "container page" ]
            [ Html.text "Student Dashboard"
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ viewBarChart session model.barChartModel
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ viewLineChart session model.lineChartModel
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ viewPieChart session model.pieChartModel
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ viewErrorMessages model.errorMessages ]
                ]
            ]
        ]


viewErrorMessages : List String -> Html Msg
viewErrorMessages errorMessages =
    div [] (List.intersperse (br [] []) (List.map text errorMessages))
