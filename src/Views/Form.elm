module Views.Form exposing (input, numberInput, password, radio, textarea, viewErrors)

import Html exposing (Attribute, Html, fieldset, li, text, ul)
import Html.Attributes exposing (class, type_)
import Html.Events


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    control Html.input ([ type_ "password" ] ++ attrs)


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    control Html.input ([ type_ "text" ] ++ attrs)


numberInput : List (Attribute msg) -> List (Html msg) -> Html msg
numberInput attrs =
    control Html.input ([ type_ "number" ] ++ attrs)


radio : String -> List String -> (v -> msg) -> List v -> List (Attribute msg) -> List (Html msg) -> Html msg
radio name values msg msgValues attrs children =
    fieldset [ class "form-group" ] <|
        List.map2
            (\strValue ->
                \msgValue ->
                    Html.label []
                        [ Html.input
                            (class
                                "form-control"
                                :: class "radio"
                                :: Html.Attributes.name name
                                :: type_ "radio"
                                :: Html.Attributes.value strValue
                                :: Html.Events.onClick (msg msgValue)
                                :: attrs
                            )
                            children
                        , text strValue
                        ]
            )
            values
            msgValues


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea =
    control Html.textarea


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "error-messages" ]



-- INTERNAL --


control :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
control element attributes children =
    fieldset [ class "form-group" ]
        [ element (class "form-control" :: attributes) children ]
