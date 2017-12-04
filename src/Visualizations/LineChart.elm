module Visualizations.LineChart exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import SampleData exposing (studentPerformanceInClass, subjectPerformance, uniteSuccessAndTime)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, Scale, defaultBandConfig)
import Visualization.Shape as Shape


type alias Model =
    List ( String, ( Float, Float ) )


model : Model
model =
    studentPerformanceInClass


w : Float
w =
    900


h : Float
h =
    450



-- height not including padding for axes


realH : Float
realH =
    h - 2 * padding


padding : Float
padding =
    30


scaleHeight : Float
scaleHeight =
    1


xScale : List ( String, ( Float, Float ) ) -> BandScale String
xScale model =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first model) ( 0, w - 2 * padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, scaleHeight ) ( realH, 0 )


xAxis : List ( String, ( Float, Float ) ) -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickCount = List.length model } (Scale.toRenderable (xScale model))


yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 10 } yScale


transformToLineData : ( String, ( Float, Float ) ) -> Maybe ( Float, Float )
transformToLineData ( student_name, ( success_percentage, avg_time_in_secs ) ) =
    Just ( Scale.convert (xScale model) student_name, Scale.convert yScale (uniteSuccessAndTime ( success_percentage, avg_time_in_secs )) )



--TODO: avg_time_in_secs


line : List ( String, ( Float, Float ) ) -> Attribute msg
line model =
    List.map transformToLineData model
        |> Shape.line Shape.linearCurve
        --Shape.monotoneInXCurve
        |> SvgAttr.d


viewLineChart : Session -> Model -> Html msg
viewLineChart session model =
    Svg.svg [ SvgAttr.width (toString w ++ "px"), SvgAttr.height (toString h ++ "px") ]
        [ Svg.g [ SvgAttr.transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis model ]
        , Svg.g [ SvgAttr.transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis ]
        , Svg.g [ SvgAttr.transform ("translate(" ++ toString (padding + padding + padding / 2) ++ ", " ++ toString padding ++ ")") ]
            [ --Svg.path [ area model, stroke "none", strokeWidth "3px", fill "rgba(255, 0, 0, 0.54)" ] []
              Svg.path [ line model, SvgAttr.stroke "red", SvgAttr.strokeWidth "3px", SvgAttr.fill "none" ] []
            ]
        ]
