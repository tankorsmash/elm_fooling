module ElmUIPlayground exposing (Model, Msg, init, update, view)

import Color
import Color.Convert as Convert
import Element exposing (Color, Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, fillPortion, height, padding, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html


type Msg
    = Click
    | Increment
    | ShowModal (Maybe String)


type alias Model =
    { rounded_edges : Int, show_modal : Bool, modal_text : Maybe String }


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
    { rounded_edges = 3
    , show_modal = False
    , modal_text = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )

        Increment ->
            ( { model | rounded_edges = model.rounded_edges + 10 }, Cmd.none )

        ShowModal maybe_str ->
            ( { model | show_modal = not model.show_modal, modal_text = maybe_str }, Cmd.none )


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


modal : Maybe String -> Element.Element Msg
modal maybe_str =
    let
        str =
            case maybe_str of
                Just str_ ->
                    str_

                Nothing ->
                    "IN FRONT"
    in
    row
        [ width fill
        , height fill
        , Font.center
        , Background.color <| rgb 1 0 0
        , Events.onClick <| ShowModal Nothing

        -- , Element.explain Debug.todo
        ]
        [ column [ centerX ] [ text str ] ]


view : Model -> Html.Html Msg
view model =
    let
        in_front =
            if model.show_modal then
                Element.inFront <| modal model.modal_text

            else
                -- TODO come up with a better way to return nothing from this
                Element.inFront <| el [] <| text ""
    in
    Element.layout [ in_front ] <|
        column [ width fill, centerY ]
            [ myRowOfStuff model
            , row [ width fill ]
                [ row [ width <| fillPortion 2 ] []
                , column [ width <| fillPortion 1 ]
                    [ primary_button [] (ShowModal Nothing) "Click Me"
                    ]
                , column [ width <| fillPortion 1 ]
                    [ primary_button [] (ShowModal <| Just "OMFG \nModal Text!!!") "Modal With Text"
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