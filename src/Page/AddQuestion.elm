module Page.AddQuestion exposing (..)

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


type alias Model =
    { successMessage : String
    , errorMessages : List String
    , lastExport : String
    , curriculums : List String
    , selectedCurriculum : String
    , selectedSubjects : List String
    , subjectsInCurriculums : Dict String (List String)
    }


model : Model
model =
    { successMessage = ""
    , errorMessages = []
    , lastExport = ""
    , curriculums = []
    , selectedCurriculum = ""
    , selectedSubjects = []
    , subjectsInCurriculums = Dict.empty
    }


type Msg
    = MyScriptExport String
    | SelectCurriculum String
    | SelectSubjects (List String)
    | AddQuestion
    | AddQuestionResult (Result Http.Error ResponseData)
    | LoadSubjectsInCurriculums (Result Http.Error SubjectsInCurriculums)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        MyScriptExport str ->
            { model | lastExport = Debug.log "latestExport" str }
                ! []

        SelectCurriculum str ->
            { model | selectedCurriculum = str } ! []

        SelectSubjects strList ->
            { model | selectedSubjects = strList } ! []

        AddQuestion ->
            model ! [ addQuestion session model ]

        AddQuestionResult (Err httpError) ->
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

        AddQuestionResult (Ok resp) ->
            let
                newModel =
                    { model | successMessage = "Question added successfuly." }
            in
            newModel => Cmd.none

        LoadSubjectsInCurriculums (Ok resp) ->
            { model | curriculums = Dict.keys resp.subjectsInCurriculums, subjectsInCurriculums = resp.subjectsInCurriculums } ! []

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


type alias Question =
    { mathml : String
    }


type alias Solution =
    { mathml : String
    }


type alias ResponseData =
    { success : Bool
    , error_messages : List String
    }


addQuestion : Session -> Model -> Cmd Msg
addQuestion session model =
    httpPost "add_question" ( session, model ) addQuestionRequestEncoder responseDecoder AddQuestionResult


questionFromModel : Model -> Question
questionFromModel model =
    Question model.lastExport


questionEncoder : Question -> JE.Value
questionEncoder question =
    JE.object
        [ ( "mathml", JE.string question.mathml )
        ]


addQuestionRequestEncoder : ( Session, Model ) -> JE.Value
addQuestionRequestEncoder ( session, model ) =
    JE.object
        [ ( "question", questionEncoder (questionFromModel model) )
        , ( "sid"
          , case session.user of
                Just user ->
                    Data.AuthToken.encode user.token

                Nothing ->
                    JE.null
          )
        , ( "subjects", JE.list (List.map JE.string model.selectedSubjects) )
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
    div [ class "add-question-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ text "Curriculum:"
                , select [ onInput SelectCurriculum ] (List.map (\x -> option [] [ text x ]) model.curriculums)
                , text "Pick subjects (Hold Ctrl to select multiple subjects) this question fits in:"
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
            , div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
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
                    , text model.successMessage
                    , button [ class "btn btn-lg btn-primary pull-xs-right", onClick AddQuestion ]
                        [ text "Add Question" ]
                    ]
                ]
            ]
        ]


onExport : (String -> msg) -> Attribute msg
onExport message =
    on "exports-changed" (JD.map message (JD.at [ "detail", "value", "application/mathml+xml" ] JD.string))
