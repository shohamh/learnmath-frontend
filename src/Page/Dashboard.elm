module Page.Dashboard exposing (Model, Msg, model, update, view)

import Data.Session as Session exposing (Session)
import Data.User as User
import Date exposing (Date)
import Date.Extra as Date
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import SampleData exposing (timeSeries)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


type Msg
    = NoOp


type alias Model =
    { dataModel : List ( Date, Float ) }


model : Model
model =
    { dataModel = timeSeries }


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : List ( Date, Float ) -> BandScale Date
xScale model =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first model) ( 0, w - 2 * padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 5 ) ( h - 2 * padding, 0 )


xAxis : List ( Date, Float ) -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just (Date.toFormattedString "dd MMM") } (Scale.toRenderable (xScale model))


yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } yScale


column : BandScale Date -> ( Date, Float ) -> Svg msg
column xScale ( date, value ) =
    Svg.g [ SvgAttr.class "column" ]
        [ Svg.rect
            [ SvgAttr.x <| toString <| Scale.convert xScale date
            , SvgAttr.y <| toString <| Scale.convert yScale value
            , SvgAttr.width <| toString <| Scale.bandwidth xScale
            , SvgAttr.height <| toString <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , Svg.text_
            [ SvgAttr.x <| toString <| Scale.convert (Scale.toRenderable xScale) date
            , SvgAttr.y <| toString <| Scale.convert yScale value - 5
            , SvgAttr.textAnchor "middle"
            ]
            [ Svg.text <| toString value ]
        ]


view : Session -> Model -> Html Msg
view session model =
    div [ class "dashboard-page" ]
        [ div [ class "container page" ]
            [ Html.text "Student Dashboard"
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ Svg.svg [ SvgAttr.width (toString w ++ "px"), SvgAttr.height (toString h ++ "px") ]
                        [ Svg.style [] [ Svg.text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
                        , Svg.g [ SvgAttr.transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                            [ xAxis model.dataModel ]
                        , Svg.g [ SvgAttr.transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                            [ yAxis ]
                        , Svg.g [ SvgAttr.transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), SvgAttr.class "series" ] <|
                            List.map (column (xScale model.dataModel)) model.dataModel
                        , Svg.text "svg hey"
                        ]
                    ]
                ]
            ]
        ]


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            model ! []
