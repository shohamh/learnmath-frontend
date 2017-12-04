module Visualizations.BarChart exposing (..)

import Data.Session as Session exposing (Session)
import Data.Subject exposing (Subject(..))
import Data.User as User
import Date exposing (Date)
import Date.Extra as Date
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import SampleData exposing (studentPerformanceInClass, subjectPerformance)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.List
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, Scale, defaultBandConfig)


type alias Model =
    List ( Subject, ( Float, Float ) )


model : Model
model =
    subjectPerformance


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


xScale : List ( Subject, ( Float, Float ) ) -> BandScale Subject
xScale model =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first model) ( 0, w - 2 * padding )


scaleHeight : Float
scaleHeight =
    20


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, scaleHeight ) ( realH, 0 )


xAxis : List ( Subject, ( Float, Float ) ) -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just Data.Subject.toString } (Scale.toRenderable (xScale model))


yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left } yScale


column : BandScale Subject -> ( Subject, ( Float, Float ) ) -> Svg msg
column xScale ( subject, ( correct, wrong ) ) =
    let
        bottomRectHeight =
            Scale.convert yScale fixedCorrect

        fixedCorrect =
            scaleHeight - correct

        fixedWrong =
            scaleHeight - wrong
    in
    Svg.g [ SvgAttr.class "column" ]
        [ Svg.rect
            [ SvgAttr.x <| toString <| Scale.convert xScale subject
            , SvgAttr.y <| toString <| Debug.log "bottom y" (realH - Scale.convert yScale fixedCorrect)
            , SvgAttr.width <| toString <| Scale.bandwidth xScale
            , SvgAttr.height <| toString <| Debug.log "bottom height" (Scale.convert yScale fixedCorrect)
            , SvgAttr.class "bottomrect"
            ]
            []
        , Svg.rect
            [ SvgAttr.x <| toString <| Scale.convert xScale subject
            , SvgAttr.y <| toString <| Debug.log "upper y" (realH - (bottomRectHeight + Scale.convert yScale fixedWrong))
            , SvgAttr.width <| toString <| Scale.bandwidth xScale
            , SvgAttr.height <| toString <| Debug.log "upper height" (Scale.convert yScale fixedWrong)
            , SvgAttr.class "upperrect"
            ]
            []
        , Svg.text_
            [ SvgAttr.x <| toString <| Scale.convert (Scale.toRenderable xScale) subject
            , SvgAttr.y <| toString <| realH - Scale.convert yScale (scaleHeight - (correct + wrong)) - padding / 2
            , SvgAttr.textAnchor "middle"
            ]
            [ Svg.text <| toString (correct + wrong) ]
        ]


viewBarChart : Session -> Model -> Html msg
viewBarChart session model =
    Svg.svg [ SvgAttr.width (toString w ++ "px"), SvgAttr.height (toString h ++ "px") ]
        [ Svg.style [] [ Svg.text """
            .column rect.bottomrect { fill: rgba(118, 214, 78, 0.8); }
            .column rect.upperrect { fill: rgba(242, 14, 14, 0.8); }
            .column:hover rect.bottomrect { fill: rgb(118, 214, 78); }
            .column:hover rect.upperrect { fill: rgb(242, 14, 14); }
            .column text { display: none; }
            .column:hover text { display: inline; }
          """ ]
        , Svg.g [ SvgAttr.transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis model ]
        , Svg.g [ SvgAttr.transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis ]
        , Svg.g [ SvgAttr.transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), SvgAttr.class "series" ] <|
            List.map (column (xScale model)) model
        ]
