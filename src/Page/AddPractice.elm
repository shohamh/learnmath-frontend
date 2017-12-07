module Page.AddPractice exposing (..)

import Data.AuthToken
import Data.Session as Session exposing (Session)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JDP exposing (decode, required)
import Json.Encode as JE exposing (..)
import MultiSelect exposing (multiSelect)
import Util exposing ((=>), httpPost)
import Views.Form


type alias Model =
    { successMessage : String
    , errorMessages : List String
    , curriculums : List String
    , selectedCurriculum : String
    , selectedSubjects : List String
    , numberOfQuestions : Int
    , subjectsInCurriculums : Dict String (List String)
    }


model : Model
model =
    { successMessage = ""
    , errorMessages = []
    , curriculums = []
    , selectedCurriculum = ""
    , selectedSubjects = []
    , numberOfQuestions = 10
    , subjectsInCurriculums = Dict.empty
    }


type Msg
    = SelectCurriculum String
    | SelectSubjects (List String)
    | UpdateNumberOfQuestions String
    | AddPractice
    | AddPracticeResult (Result Http.Error ResponseData)
    | LoadSubjectsInCurriculums (Result Http.Error SubjectsInCurriculums)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SelectCurriculum str ->
            { model | selectedCurriculum = str } ! []

        SelectSubjects strList ->
            { model | selectedSubjects = strList } ! []

        UpdateNumberOfQuestions str ->
            { model | numberOfQuestions = Result.withDefault 10 (String.toInt str) } ! []

        AddPractice ->
            model ! [ addPractice session model ]

        AddPracticeResult (Err httpError) ->
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

        AddPracticeResult (Ok resp) ->
            let
                newModel =
                    { model | successMessage = "Practice added successfully to all relevant students." }
            in
            newModel => Cmd.none

        LoadSubjectsInCurriculums (Ok resp) ->
            Debug.log "loadsubjectsinCurriculums model"
                { model | curriculums = Dict.keys (Debug.log "curriculums:" resp.subjectsInCurriculums), subjectsInCurriculums = resp.subjectsInCurriculums }
                ! []

        LoadSubjectsInCurriculums (Err httpError) ->
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


type alias ResponseData =
    { success : Bool
    , error_messages : List String
    }


addPractice : Session -> Model -> Cmd Msg
addPractice session model =
    httpPost "create_practice_session" ( session, model ) addPracticeRequestEncoder responseDecoder AddPracticeResult


addPracticeRequestEncoder : ( Session, Model ) -> JE.Value
addPracticeRequestEncoder ( session, model ) =
    JE.object
        [ ( "sid"
          , case session.user of
                Just user ->
                    Data.AuthToken.encode user.token

                Nothing ->
                    JE.null
          )
        , ( "subjects", JE.list (List.map JE.string model.selectedSubjects) )
        , ( "question_count", JE.int model.numberOfQuestions )
        ]


loadCurriculumsAndSubjects : Session -> Model -> Cmd Msg
loadCurriculumsAndSubjects session model =
    let
        sid =
            case session.user of
                Just user ->
                    Data.AuthToken.toString user.token

                Nothing ->
                    ""
    in
    httpPost "subjects_in_all_curriculums" sid loadCurriculumsAndSubjectsEncoder loadCursAndSubjectsRespDecoder LoadSubjectsInCurriculums


loadCurriculumsAndSubjectsEncoder : String -> JE.Value
loadCurriculumsAndSubjectsEncoder sid =
    JE.object [ ( "sid", JE.string sid ) ]


type alias SubjectsInCurriculums =
    { subjectsInCurriculums : Dict String (List String)
    , success : Bool
    , error_messages : List String
    }


loadCursAndSubjectsRespDecoder : Decoder SubjectsInCurriculums
loadCursAndSubjectsRespDecoder =
    JDP.decode SubjectsInCurriculums
        |> JDP.required "subjects_in_curriculums" (JD.dict (JD.list JD.string))
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)


responseDecoder : Decoder ResponseData
responseDecoder =
    JDP.decode ResponseData
        |> JDP.required "success" JD.bool
        |> JDP.required "error_messages" (JD.list JD.string)


viewErrorMessages : List String -> Html Msg
viewErrorMessages errorMessages =
    div [] (List.intersperse (br [] []) (List.map text errorMessages))


subs : Model -> Sub Msg
subs model =
    Sub.none


view : Session -> Model -> Html Msg
view session model =
    div [ class "add-practice-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div []
                    [ text "Curriculum:"
                    , br [] []
                    , select [ onInput SelectCurriculum ] (List.map (\x -> option [] [ text x ]) model.curriculums)
                    ]
                , div []
                    [ text "Pick subjects (Hold Ctrl to select multiple subjects) for the students to practice in this session:"
                    , br [] []
                    , let
                        items =
                            case Dict.get model.selectedCurriculum model.subjectsInCurriculums of
                                Just list ->
                                    list

                                Nothing ->
                                    []
                      in
                      multiSelect
                        (MultiSelect.Options
                            (List.map (\x -> MultiSelect.Item x x True) items)
                            SelectSubjects
                        )
                        []
                        model.selectedSubjects
                    ]
                , div []
                    [ text "Number of questions:"
                    , br [] []
                    , Views.Form.numberInput [ Html.Attributes.value (Basics.toString model.numberOfQuestions), onInput UpdateNumberOfQuestions ] []
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    [ button [ class "btn btn-lg btn-primary pull-xs-right", onClick AddPractice ]
                        [ text "Add Practice Session" ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    [ viewErrorMessages model.errorMessages
                    , text model.successMessage
                    ]
                ]
            ]
        ]
