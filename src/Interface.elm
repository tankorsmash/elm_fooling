module Interface exposing (ButtonConfig, ButtonCustomParams, ButtonParams(..), ButtonTextParams, ButtonType(..), ColorTheme(..), Device, DeviceClass(..), HoveredTooltip(..), Orientation(..), StandardButton, TooltipBody(..), TooltipConfig, TooltipData, TooltipId, TooltipMsg(..), addButtonAttrs, blankChar, buildTooltipElementConfig, buildTooltipTextConfig, button, buttonWithTooltip, classifyDevice, clipText, colorFromInt, color_black, color_danger, color_danger_bright, color_grey, color_light_grey, color_off_black, color_pastel_green_1, color_pastel_green_2, color_pastel_green_3, color_pastel_green_4, color_pastel_green_5, color_pastel_green_6, color_pastel_green_7, color_pastel_red_1, color_pastel_red_2, color_pastel_red_3, color_pastel_red_4, color_pastel_red_5, color_pastel_red_6, color_pastel_red_7, color_primary, color_secondary, color_secondary_bright, color_ultra_light_grey, color_very_light_grey, color_very_very_light_grey, color_white, common_button_attrs, convertColor, cssRule, dangerButtonConfig, decodeColorTheme, defaultBackgroundColor, defaultFontColor, defaultRounded, defaultSolidColor, defaultTextColor, defineHtmlId, deviceClassToString, encodeColorTheme, font_blood, font_grey, font_scaled, getButtonConfig, getTooltipOffset, hex_to_color, hoveredTooltipMatchesId, monospace, nbsp, noUserSelect, orientationToString, outlineButtonConfig, outlineCustomAttrs, outline_button, outline_button_custom, pointerEventsAll, pointerEventsNone, primaryButtonConfig, primary_button, primary_button_custom, primary_button_tooltip, primary_color_bright, renderBlood, renderBlood_sized, renderBlood_string, renderGp, renderGpSized, renderGpString, scaled, scrollbarYEl, secondaryButtonConfig, tooltipElem, wrapButtonWithTooltip)

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


encodeColorTheme : ColorTheme -> Decode.Value
encodeColorTheme colorTheme =
    case colorTheme of
        BrightTheme ->
            Encode.string "BrightTheme"

        DarkTheme ->
            Encode.string "DarkTheme"


decodeColorTheme : Decoder ColorTheme
decodeColorTheme =
    Decode.string
        |> Decode.andThen
            (\colorStr ->
                case colorStr of
                    "BrightTheme" ->
                        Decode.succeed BrightTheme

                    "DarkTheme" ->
                        Decode.succeed DarkTheme

                    _ ->
                        Decode.fail "color theme is not recognized"
            )


type alias TooltipData =
    { offsetX : Float
    , offsetY : Float
    , hoveredTooltipId : String
    }


type HoveredTooltip
    = NoHoveredTooltip
    | HoveredTooltipWithoutOffset TooltipData
    | HoveredTooltipWithOffset TooltipData


type TooltipBody msg
    = TooltipText String
    | TooltipElement (Element msg)


type alias TooltipConfig msg =
    { tooltip_id : String
    , tooltip_body : TooltipBody msg
    , onTooltipMsg : TooltipMsg -> msg
    }


type TooltipMsg
    = StartTooltipHover String
    | EndTooltipHover String


type ButtonType
    = Primary
    | Secondary
    | Outline
    | Danger


type alias ButtonTextParams msg =
    { buttonType : ButtonType
    , colorTheme : ColorTheme
    , customAttrs : List (Element.Attribute msg)
    , textLabel : String
    , onPressMsg : msg
    }


type alias ButtonCustomParams msg =
    { buttonType : ButtonType
    , colorTheme : ColorTheme
    , customAttrs : List (Element.Attribute msg)
    , customLabel : Element msg
    , onPressMsg : msg
    }


type ButtonParams msg
    = TextParams (ButtonTextParams msg)
    | CustomParams (ButtonCustomParams msg)


getButtonConfig : ButtonType -> ButtonConfig
getButtonConfig buttonType =
    case buttonType of
        Primary ->
            primaryButtonConfig

        Secondary ->
            secondaryButtonConfig

        Outline ->
            outlineButtonConfig

        Danger ->
            dangerButtonConfig


type alias StandardButton msg =
    ButtonTextParams msg -> Element msg


getTooltipOffset : HoveredTooltip -> { offsetX : Float, offsetY : Float }
getTooltipOffset hoveredTooltip =
    case hoveredTooltip of
        HoveredTooltipWithOffset data ->
            { offsetX = data.offsetX, offsetY = data.offsetY }

        HoveredTooltipWithoutOffset cached_data ->
            { offsetX = cached_data.offsetX, offsetY = cached_data.offsetY }

        _ ->
            { offsetX = 0, offsetY = 0 }


type alias TooltipId =
    String


{-| -}
type alias Device =
    { class : DeviceClass
    , orientation : Orientation
    , size : { width : Int, height : Int }
    }


{-| -}
type DeviceClass
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


deviceClassToString : DeviceClass -> String
deviceClassToString deviceClass =
    case deviceClass of
        Phone ->
            "Phone"

        Tablet ->
            "Tablet"

        Desktop ->
            "Desktop"

        BigDesktop ->
            "BigDesktop"


{-| -}
type Orientation
    = Portrait
    | Landscape


orientationToString : Orientation -> String
orientationToString orientation =
    case orientation of
        Portrait ->
            "Portrait"

        Landscape ->
            "Landscape"


{-| -}
classifyDevice : { height : Int, width : Int } -> Device
classifyDevice window =
    -- Tested in this ellie:
    -- https://ellie-app.com/68QM7wLW8b9a1
    { size = window
    , class =
        let
            longSide =
                max window.width window.height

            shortSide =
                min window.width window.height
        in
        if shortSide < 600 then
            Phone

        else if longSide <= 1200 then
            Tablet

        else if longSide > 1200 && longSide <= 1920 then
            Desktop

        else
            BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    }


color_white : Color
color_white =
    rgb 1 1 1


color_black : Color
color_black =
    rgb 0 0 0


color_off_black : Color
color_off_black =
    hex_to_color "#0a0a0a"


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


color_secondary_bright : Color
color_secondary_bright =
    convertColor Color.lightCharcoal


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
        , Font.color <| hovered_font_color
        ]
    ]


{-| 'custom' renderer, instead of text
-}
primary_button_custom : ButtonTextParams msg -> Element msg -> Element msg
primary_button_custom { customAttrs, onPressMsg } label =
    Input.button
        (common_button_attrs
            { font_color = color_white
            , button_color = color_primary
            , hovered_button_color = primary_color_bright
            , hovered_font_color = color_white
            }
            ++ customAttrs
        )
        { onPress = Just onPressMsg, label = label }


primary_button : ButtonTextParams msg -> Element msg
primary_button ({ textLabel } as buttonParams) =
    primary_button_custom buttonParams (text textLabel)


primaryButtonConfig : ButtonConfig
primaryButtonConfig =
    { font_color = color_white
    , button_color = color_primary
    , hovered_button_color = primary_color_bright
    , hovered_font_color = color_white
    }


secondaryButtonConfig : ButtonConfig
secondaryButtonConfig =
    { font_color = color_white
    , button_color = color_secondary
    , hovered_button_color = color_secondary_bright
    , hovered_font_color = color_white
    }


outlineButtonConfig : ButtonConfig
outlineButtonConfig =
    { font_color = color_secondary
    , button_color = color_white
    , hovered_button_color = color_secondary
    , hovered_font_color = color_white
    }


outlineCustomAttrs : List (Element.Attribute msg)
outlineCustomAttrs =
    [ Border.rounded 5
    , Border.width 2
    , Border.color color_secondary
    , padding 9
    ]


addButtonAttrs : ButtonType -> List (Element.Attribute msg) -> List (Element.Attribute msg)
addButtonAttrs buttonType customAttrs =
    common_button_attrs (getButtonConfig buttonType)
        ++ (if buttonType == Outline then
                outlineCustomAttrs

            else
                []
           )
        ++ customAttrs


button : ButtonParams msg -> Element msg
button params =
    case params of
        TextParams { buttonType, customAttrs, onPressMsg, textLabel } ->
            Input.button
                (addButtonAttrs buttonType customAttrs)
                { onPress = Just onPressMsg, label = text textLabel }

        CustomParams { buttonType, customAttrs, onPressMsg, customLabel } ->
            Input.button
                (addButtonAttrs buttonType customAttrs)
                { onPress = Just onPressMsg, label = customLabel }


buttonWithTooltip : ButtonParams msg -> TooltipConfig msg -> HoveredTooltip -> Element msg
buttonWithTooltip params { tooltip_id, tooltip_body, onTooltipMsg } hoveredTooltip =
    let
        tooltip_el =
            \clrTheme -> tooltipElem clrTheme tooltip_id hoveredTooltip tooltip_body

        mouseAttrs =
            [ Events.onMouseLeave <| onTooltipMsg <| EndTooltipHover tooltip_id
            , Events.onMouseEnter <| onTooltipMsg <| StartTooltipHover tooltip_id
            ]

        tooltipAttr clrTheme =
            if hoveredTooltipMatchesId hoveredTooltip tooltip_id then
                [ Element.above (tooltip_el clrTheme)
                ]
                    ++ mouseAttrs

            else
                mouseAttrs
    in
    case params of
        TextParams { colorTheme, buttonType, customAttrs, onPressMsg, textLabel } ->
            Input.button
                (addButtonAttrs buttonType (tooltipAttr colorTheme ++ customAttrs))
                { onPress = Just onPressMsg, label = text textLabel }

        CustomParams { colorTheme, buttonType, customAttrs, onPressMsg, customLabel } ->
            Input.button
                (addButtonAttrs buttonType (tooltipAttr colorTheme ++ customAttrs))
                { onPress = Just onPressMsg, label = customLabel }


outline_button_custom : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
outline_button_custom customAttrs onPressMsg label =
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
            ++ customAttrs
        )
        { onPress = Just onPressMsg, label = label }


outline_button : List (Element.Attribute msg) -> msg -> String -> Element msg
outline_button customAttrs onPressMsg label =
    button <|
        TextParams
            { colorTheme = BrightTheme
            , onPressMsg = onPressMsg
            , textLabel = label
            , buttonType = Outline
            , customAttrs = customAttrs
            }



-- this doesn't help me so far


scrollbarYEl : List (Element.Attribute msg) -> Element msg -> Element msg
scrollbarYEl customAttrs body =
    el [ height fill, width fill ] <|
        el
            ([ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
             , Element.htmlAttribute <| Html.Attributes.style "top" "0"
             , Element.htmlAttribute <| Html.Attributes.style "right" "0"
             , Element.htmlAttribute <| Html.Attributes.style "bottom" "0"
             , Element.htmlAttribute <| Html.Attributes.style "left" "0"
             , Element.scrollbarY
             ]
                ++ customAttrs
            )
            body


dangerButtonConfig : ButtonConfig
dangerButtonConfig =
    { font_color = color_white
    , button_color = color_danger
    , hovered_button_color = color_danger_bright
    , hovered_font_color = color_white
    }


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


renderGp : ColorTheme -> Int -> Element msg
renderGp colorTheme count =
    renderGpSized colorTheme count 12


renderGpString : Int -> String
renderGpString count =
    String.fromInt count ++ "gp"


renderGpSized : ColorTheme -> Int -> Int -> Element msg
renderGpSized colorTheme count font_size =
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


nbsp : String
nbsp =
    "\u{00A0}"


blankChar =
    "\u{2003}"


cssRule : String -> String -> Element.Attribute msg
cssRule name value =
    Html.Attributes.style name value |> Element.htmlAttribute


defineHtmlId : String -> Element.Attribute msg
defineHtmlId name =
    Html.Attributes.id name |> Element.htmlAttribute


noUserSelect : Element.Attribute msg
noUserSelect =
    Html.Attributes.style "userSelect" "none" |> Element.htmlAttribute


pointerEventsNone : Element.Attribute msg
pointerEventsNone =
    Html.Attributes.style "pointer-events" "none" |> Element.htmlAttribute


pointerEventsAll : Element.Attribute msg
pointerEventsAll =
    Html.Attributes.style "pointer-events" "all" |> Element.htmlAttribute


hoveredTooltipMatchesId : HoveredTooltip -> String -> Bool
hoveredTooltipMatchesId hoveredTooltip tooltip_id =
    case hoveredTooltip of
        HoveredTooltipWithoutOffset tooltipData ->
            if tooltipData.hoveredTooltipId == tooltip_id then
                True

            else
                False

        HoveredTooltipWithOffset hoveredTooltipData ->
            if hoveredTooltipData.hoveredTooltipId == tooltip_id then
                True

            else
                False

        NoHoveredTooltip ->
            False


scaled : Int -> Int
scaled val =
    modular 14 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale


tooltipElem : ColorTheme -> String -> HoveredTooltip -> TooltipBody msg -> Element msg
tooltipElem colorTheme tooltip_id hoveredTooltip tooltip_body =
    let
        { offsetX, offsetY } =
            getTooltipOffset hoveredTooltip
    in
    Element.el
        [ width Element.shrink
        , defaultFontColor colorTheme
        , defaultBackgroundColor colorTheme
        , Border.color <| convertColor Color.charcoal
        , Border.rounded 3
        , Border.width 2
        , padding 10
        , if offsetY == 0 then
            Element.moveUp 20

          else
            Element.moveDown offsetY
        , Element.moveRight offsetX
        , centerX
        , Element.htmlAttribute <|
            Html.Attributes.id ("tooltip__" ++ tooltip_id)
        ]
    <|
        case tooltip_body of
            TooltipText tt_text ->
                text tt_text

            TooltipElement elem ->
                elem


wrapButtonWithTooltip : ButtonTextParams msg -> TooltipConfig msg -> HoveredTooltip -> Element msg
wrapButtonWithTooltip { buttonType, colorTheme, customAttrs, onPressMsg, textLabel } { onTooltipMsg, tooltip_id, tooltip_body } hoveredTooltip =
    let
        tooltip_el =
            tooltipElem colorTheme tooltip_id hoveredTooltip tooltip_body

        tooltipAttr =
            if hoveredTooltipMatchesId hoveredTooltip tooltip_id then
                [ Element.above tooltip_el ]

            else
                []
    in
    button <|
        TextParams
            { buttonType = buttonType
            , customAttrs =
                [ Events.onMouseLeave <| onTooltipMsg <| EndTooltipHover tooltip_id
                , Events.onMouseEnter <| onTooltipMsg <| StartTooltipHover tooltip_id
                ]
                    ++ tooltipAttr
                    ++ customAttrs
            , onPressMsg = onPressMsg
            , colorTheme = colorTheme
            , textLabel = textLabel
            }


primary_button_tooltip :
    ButtonTextParams msg
    -> TooltipConfig msg
    -> HoveredTooltip
    -> Element msg
primary_button_tooltip { colorTheme, customAttrs, onPressMsg, textLabel } { onTooltipMsg, tooltip_id, tooltip_body } hoveredTooltip =
    let
        tooltip_el =
            tooltipElem colorTheme tooltip_id hoveredTooltip tooltip_body

        tooltipAttr =
            if hoveredTooltipMatchesId hoveredTooltip tooltip_id then
                [ Element.above tooltip_el ]

            else
                []
    in
    primary_button
        { buttonType = Primary
        , colorTheme = colorTheme
        , customAttrs =
            [ Events.onMouseLeave <| onTooltipMsg <| EndTooltipHover tooltip_id
            , Events.onMouseEnter <| onTooltipMsg <| StartTooltipHover tooltip_id
            ]
                ++ tooltipAttr
                ++ customAttrs
        , onPressMsg = onPressMsg
        , textLabel = textLabel
        }


buildTooltipTextConfig : String -> (TooltipMsg -> msg) -> TooltipConfig msg
buildTooltipTextConfig text onTooltipMsg =
    { tooltip_id = UUID.forName text UUID.dnsNamespace |> UUID.toString
    , tooltip_body = TooltipText text
    , onTooltipMsg = onTooltipMsg
    }


buildTooltipElementConfig : TooltipId -> Element msg -> (TooltipMsg -> msg) -> TooltipConfig msg
buildTooltipElementConfig tooltip_id element onTooltipMsg =
    { tooltip_id = UUID.forName tooltip_id UUID.dnsNamespace |> UUID.toString
    , tooltip_body = TooltipElement element
    , onTooltipMsg = onTooltipMsg
    }


defaultFontColor : ColorTheme -> Element.Attribute msg
defaultFontColor colorTheme =
    defaultTextColor colorTheme
        |> Font.color


defaultSolidColor : ColorTheme -> Color
defaultSolidColor colorTheme =
    case colorTheme of
        BrightTheme ->
            convertColor Color.white

        DarkTheme ->
            convertColor Color.darkCharcoal


defaultBackgroundColor : ColorTheme -> Element.Attribute msg
defaultBackgroundColor colorTheme =
    defaultSolidColor colorTheme
        |> Background.color


defaultTextColor : ColorTheme -> Color
defaultTextColor colorTheme =
    case colorTheme of
        BrightTheme ->
            convertColor Color.black

        DarkTheme ->
            hex_to_color "#ccc"


defaultRounded =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }
