module Interface exposing (..)

import Array
import Browser.Dom
import Browser.Events
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Color
import Color.Convert as Convert
import DOM exposing (offsetWidth, target)
import Dict
import Element
    exposing
        ( Color
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , modular
        , padding
        , paddingXY
        , paragraph
        , rgb
        , rgb255
        , row
        , scrollbars
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy as Lazy
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Html
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Random
import Random.List
import Task
import Test exposing (..)
import Time
import Tuple3
import UUID exposing (UUID)


type ColorTheme
    = BrightTheme
    | DarkTheme


color_white : Color
color_white =
    rgb 1 1 1


color_black : Color
color_black =
    rgb 0 0 0


hex_to_color : String -> Color
hex_to_color hex_str =
    case Convert.hexToColor hex_str of
        Ok color ->
            let
                -- convert to a Color lib Color record
                rgba =
                    Color.toRgba color
            in
            -- from the Color record, call the ElmUI `rgb` func
            rgb rgba.red rgba.green rgba.blue

        Err err ->
            Debug.todo "NOOO" <| rgb255 255 0 0


{-| lightest green at 1, darkest at 7
-}
color_pastel_green_1 : Color
color_pastel_green_1 =
    hex_to_color "#b4ecb4"


color_pastel_green_2 : Color
color_pastel_green_2 =
    hex_to_color "#a0e7a0"


color_pastel_green_3 : Color
color_pastel_green_3 =
    hex_to_color "#8be28b"


color_pastel_green_4 : Color
color_pastel_green_4 =
    hex_to_color "#77dd77"


color_pastel_green_5 : Color
color_pastel_green_5 =
    hex_to_color "#63d863"


color_pastel_green_6 : Color
color_pastel_green_6 =
    hex_to_color "#4ed34e"


color_pastel_green_7 : Color
color_pastel_green_7 =
    hex_to_color "#3ace3a"


{-| lightest red at 1, darkest at 7
-}
color_pastel_red_1 : Color
color_pastel_red_1 =
    hex_to_color "#ecb4b4"


color_pastel_red_2 : Color
color_pastel_red_2 =
    hex_to_color "#e7a0a0"


color_pastel_red_3 : Color
color_pastel_red_3 =
    hex_to_color "#e28b8b"


color_pastel_red_4 : Color
color_pastel_red_4 =
    hex_to_color "#dd7777"


color_pastel_red_5 : Color
color_pastel_red_5 =
    hex_to_color "#d86363"


color_pastel_red_6 : Color
color_pastel_red_6 =
    hex_to_color "#d34e4e"


color_pastel_red_7 : Color
color_pastel_red_7 =
    hex_to_color "#ce3a3a"


color_secondary : Color
color_secondary =
    convertColor Color.charcoal


color_danger : Color
color_danger =
    convertColor Color.red


color_danger_bright : Color
color_danger_bright =
    convertColor Color.lightRed


color_primary : Color
color_primary =
    convertColor Color.blue


primary_color_bright : Color
primary_color_bright =
    convertColor Color.lightBlue


type alias ButtonConfig =
    { font_color : Color
    , button_color : Color
    , hovered_button_color : Color
    , hovered_font_color : Color
    }


common_button_attrs : ButtonConfig -> List (Element.Attribute msg)
common_button_attrs { font_color, button_color, hovered_button_color, hovered_font_color } =
    [ -- bs4-like values
      Font.color font_color
    , Font.size 16
    , Font.center
    , padding 6
    , Background.color button_color
    , Border.rounded 5
    , Border.width 5
    , Border.color button_color
    , Element.mouseOver
        [ Background.color <| hovered_button_color
        , Border.color <| hovered_button_color
        , Font.color <| rgb 0 0 0
        ]
    ]


primary_button_custom : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
primary_button_custom custom_attrs on_press label =
    Input.button
        (common_button_attrs
            { font_color = color_white
            , button_color = color_primary
            , hovered_button_color = primary_color_bright
            , hovered_font_color = color_black
            }
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


primary_button : List (Element.Attribute msg) -> msg -> String -> Element msg
primary_button custom_attrs on_press label =
    primary_button_custom custom_attrs on_press (text label)


secondary_button_custom : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
secondary_button_custom custom_attrs on_press label =
    Input.button
        (common_button_attrs
            { font_color = color_white
            , button_color = color_secondary
            , hovered_button_color = color_secondary --_bright
            , hovered_font_color = color_black
            }
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


secondary_button : List (Element.Attribute msg) -> msg -> String -> Element msg
secondary_button custom_attrs on_press label =
    secondary_button_custom custom_attrs on_press (text label)


outline_button_custom : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
outline_button_custom custom_attrs on_press label =
    Input.button
        ([ -- bs4-like values
           Font.color color_secondary
         , Font.size 16
         , Font.center
         , padding 9
         , Background.color color_white
         , Border.rounded 5
         , Border.width 2
         , Border.color color_secondary
         , Element.mouseOver
            [ Background.color <| color_secondary
            , Font.color <| color_white
            ]
         ]
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


outline_button : List (Element.Attribute msg) -> msg -> String -> Element msg
outline_button custom_attrs on_press label =
    outline_button_custom custom_attrs on_press (text label)



-- this doesn't help me so far


scrollbarYEl : List (Element.Attribute msg) -> Element msg -> Element msg
scrollbarYEl custom_attrs body =
    el [ height fill, width fill ] <|
        el
            ([ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
             , Element.htmlAttribute <| Html.Attributes.style "top" "0"
             , Element.htmlAttribute <| Html.Attributes.style "right" "0"
             , Element.htmlAttribute <| Html.Attributes.style "bottom" "0"
             , Element.htmlAttribute <| Html.Attributes.style "left" "0"
             , Element.scrollbarY
             ]
                ++ custom_attrs
            )
            body


danger_button_custom : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
danger_button_custom custom_attrs on_press label =
    Input.button
        (common_button_attrs
            { font_color = color_white
            , button_color = color_danger
            , hovered_button_color = color_danger_bright
            , hovered_font_color = color_black
            }
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


danger_button : List (Element.Attribute msg) -> msg -> String -> Element msg
danger_button custom_attrs on_press label_str =
    danger_button_custom custom_attrs on_press <| text label_str



convertColor : Color.Color -> Element.Color
convertColor color =
    Element.fromRgb <| Color.toRgba <| color


colorFromInt : Int -> Color -> Color -> Color -> Color
colorFromInt int positiveColor neutralColor negativeColor =
    if int > 0 then
        positiveColor

    else if int == 0 then
        neutralColor

    else
        negativeColor

monospace attrs el =
    Element.el (Font.family [ Font.monospace ] :: attrs) el



clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


color_grey : Color
color_grey =
    rgb 0.35 0.35 0.35


color_very_light_grey : Color
color_very_light_grey =
    rgb 0.75 0.75 0.75


color_very_very_light_grey : Color
color_very_very_light_grey =
    rgb 0.85 0.85 0.85


color_ultra_light_grey : Color
color_ultra_light_grey =
    rgb 0.95 0.95 0.95


color_light_grey : Color
color_light_grey =
    rgb 0.55 0.55 0.55


font_grey : Element.Attribute msg
font_grey =
    Font.color <| color_grey


font_blood : Element.Attribute msg
font_blood =
    Font.color <| color_danger


render_gp : ColorTheme -> Int -> Element msg
render_gp colorTheme count =
    render_gp_sized colorTheme count 12


render_gp_string : ColorTheme -> Int -> String
render_gp_string colorTheme count =
    String.fromInt count ++ "gp"


render_gp_sized : ColorTheme -> Int -> Int -> Element msg
render_gp_sized colorTheme count font_size =
    paragraph []
        [ text <| String.fromInt count
        , Element.el
            [ Font.size font_size
            , case colorTheme of
                BrightTheme ->
                    font_grey

                DarkTheme ->
                    Font.color <| hex_to_color "#777439"
            ]
            (text "gp")
        ]


renderBlood : ColorTheme -> Int -> Element msg
renderBlood colorTheme count =
    renderBlood_sized colorTheme count 12


renderBlood_string : Int -> String
renderBlood_string count =
    String.fromInt count ++ "gp"


renderBlood_sized : ColorTheme -> Int -> Int -> Element msg
renderBlood_sized colorTheme count font_size =
    paragraph []
        [ text <| String.fromInt count
        , Element.el [ Font.size font_size, font_blood ] (text "blood")
        ]

