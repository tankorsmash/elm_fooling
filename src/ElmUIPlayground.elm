module ElmUIPlayground exposing (Model, Msg, init, update, view)

import Color
import Color.Convert as Convert
import Element exposing (Color, Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, fillPortion, padding, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html


type Msg
    = Click
    | Increment


type alias Model =
    { rounded_edges : Int }


white_color : Color
white_color =
    rgb 1 1 1


primary_color : Color
primary_color =
    case Convert.hexToColor "#007bff" of
        Ok color ->
            let
                -- convert to a Color lib Color record
                rgba =
                    Color.toRgba color
            in
            -- from the Color record, call the ElmUI `rgb` func
            rgb rgba.red rgba.green rgba.blue

        Err err ->
            rgb255 255 0 0


init : Model
init =
    { rounded_edges = 3 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )

        Increment ->
            ( { model | rounded_edges = model.rounded_edges + 10 }, Cmd.none )


primary_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
primary_button attrs on_press label =
    Input.button
        (attrs
            ++ [ -- bs4-like values
                 Font.color white_color
               , Font.size 16
               , Font.center
               , padding 6
               , Background.color primary_color
               , Border.rounded 5
               , Border.width 5
               , Border.color primary_color
               ]
        )
        { onPress = Just on_press, label = text label }


view : Model -> Html.Html Msg
view model =
    Element.layout [] <|
        column [ width fill, centerY ]
            [ myRowOfStuff model
            , row [ width fill ]
                [ row [ width <| fillPortion 2 ] []
                , column [ width <| fillPortion 1 ]
                    [ primary_button [] Increment "Click Me"
                    ]
                , column [ width <| fillPortion 1 ]
                    [ primary_button [] Increment "End"
                    ]
                ]
            ]


myRowOfStuff : Model -> Element msg
myRowOfStuff model =
    let
        { rounded_edges } =
            model
    in
    row [ width fill, centerY, spacing 30 ]
        [ myElement rounded_edges "aa"
        , myElement rounded_edges "ss"
        , el [ alignRight ] <|
            row [ padding 20, spacing 20 ] (List.map (myElement rounded_edges) [ "asd", "ddd", "qweee" ])
        ]


myElement : Int -> String -> Element msg
myElement rounded_edges txt =
    el
        [ Background.color (rgb255 255 50 50)
        , Font.color (rgb255 255 255 255)
        , Border.rounded rounded_edges
        , padding 30
        ]
        (text txt)
