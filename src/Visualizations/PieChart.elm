module Visualizations.PieChart exposing (..)

import Array exposing (Array)
import Data.Session as Session exposing (Session)
import Data.Subject exposing (Subject(..))
import Data.User as User
import Date exposing (Date)
import Date.Extra as Date
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import SampleData exposing (mistakeTypeFrequency, studentPerformanceInClass, subjectPerformance, uniteSuccessAndTime)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.List
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, Scale, defaultBandConfig)
import Visualization.Shape as Shape exposing (defaultPieConfig)


type alias Model =
    List ( String, Float )


model : Model
model =
    mistakeTypeFrequency


w : Float
w =
    900


h : Float
h =
    450



-- height not including padding for axes


radius : Float
radius =
    min w h / 2


colors : Array String
colors =
    Array.fromList [ "#98abc5", "#8a89a6", "#7b6888", "#6b486b", "#a05d56", "#d0743c", "#ff8c00" ]


viewPieChart : Session -> Model -> Html msg
viewPieChart session model =
    let
        pieData =
            model |> List.map Tuple.second |> Shape.pie { defaultPieConfig | outerRadius = radius }

        makeSlice index datum =
            Svg.path [ SvgAttr.d (Shape.arc datum), SvgAttr.style ("fill:" ++ (Maybe.withDefault "#000" <| Array.get index colors) ++ "; stroke: #fff;") ] []

        makeLabel slice ( label, value ) =
            Svg.text_
                [ SvgAttr.transform ("translate" ++ toString (Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }))
                , SvgAttr.dy ".35em"
                , SvgAttr.textAnchor "middle"
                ]
                [ Svg.text label ]
    in
    Svg.svg [ SvgAttr.width (toString w ++ "px"), SvgAttr.height (toString h ++ "px") ]
        [ Svg.g [ SvgAttr.transform ("translate(" ++ toString (w / 2) ++ "," ++ toString (h / 2) ++ ")") ]
            [ Svg.g [] <| List.indexedMap makeSlice pieData
            , Svg.g [] <| List.map2 makeLabel pieData model
            ]
        ]
