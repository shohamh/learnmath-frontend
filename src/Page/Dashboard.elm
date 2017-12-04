module Page.Dashboard exposing (Model, Msg, model, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import SampleData exposing (studentPerformanceInClass, subjectPerformance)
import Visualizations.BarChart as BarChart exposing (viewBarChart)
import Visualizations.LineChart as LineChart exposing (viewLineChart)
import Visualizations.PieChart as PieChart exposing (viewPieChart)


type alias Model =
    { barChartModel : BarChart.Model
    , lineChartModel : LineChart.Model
    , pieChartModel : PieChart.Model
    }


model : Model
model =
    { barChartModel = BarChart.model
    , lineChartModel = LineChart.model
    , pieChartModel = PieChart.model
    }


type Msg
    = NoOp


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
            ]
        ]


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            model ! []
