module Torrent exposing (Model, Msg, init, update, view)

import Array
import Browser.Dom
import Browser.Events
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
        , explain
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
import Fuzz exposing (Fuzzer, int, list, string)
import Html
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List.Extra
import Random
import Random.List
import Task
import Test exposing (..)
import Time
import Tuple3
import UUID exposing (UUID)


type alias Model =
    { category : Category
    , text_search : String
    , receivedQuery : Maybe String
    , receivedQueryError : Maybe Http.Error
    }


type Msg
    = OnChangeCategory Category
    | OnChangeTextSearch String
    | SubmitFilmSearch
    | SubmitTvSearch
    | ReceivedQueryResponse (Result Http.Error String)


type Category
    = NoCategory
    | Film
    | Tv


init : Model
init =
    { category = NoCategory
    , text_search = ""
    , receivedQuery = Nothing
    , receivedQueryError = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnChangeCategory new_category ->
            ( { model | category = new_category }, Cmd.none )

        OnChangeTextSearch new_text_search ->
            ( { model | text_search = new_text_search }, Cmd.none )

        SubmitFilmSearch ->
            let
                http_request =
                    Http.post
                        { url = "http://localhost:4126/torrent/search"
                        , body =
                            Encode.object
                                [ ( "query", Encode.string model.text_search ) ]
                                |> Http.jsonBody
                        , expect =
                            Http.expectJson ReceivedQueryResponse
                                (field "response" (field "query" Decode.string))
                        }
            in
            ( model, http_request )

        SubmitTvSearch ->
            let
                http_request =
                    Http.post
                        { url = "http://localhost:4126/torrent/search"
                        , body =
                            Encode.object
                                [ ( "query", Encode.string model.text_search ) ]
                                |> Http.jsonBody
                        , expect =
                            Http.expectJson ReceivedQueryResponse
                                (field "response" (field "query" Decode.string))
                        }
            in
            ( model, http_request )

        ReceivedQueryResponse query_resp ->
            case query_resp of
                Ok query ->
                    ( { model
                        | receivedQueryError = Nothing
                        , receivedQuery = Just query
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | receivedQueryError = Just err
                        , receivedQuery = Nothing
                      }
                    , Cmd.none
                    )


explain =
    Element.explain Debug.todo


viewTextSearch category model =
    Input.search []
        { onChange = OnChangeTextSearch
        , text = model.text_search
        , placeholder =
            Just <|
                Input.placeholder [] <|
                    text <|
                        case category of
                            Film ->
                                "Film name here"

                            Tv ->
                                "Tv name here"

                            NoCategory ->
                                "Name here"
        , label =
            Input.labelLeft [] <|
                text <|
                    case category of
                        Film ->
                            "Movie:"

                        Tv ->
                            "Tv:"

                        NoCategory ->
                            "Search:"
        }


viewFilmOptions model =
    row [ width fill ]
        [ viewTextSearch Film model
        ]


viewTvOptions model =
    row [ width fill ]
        [ viewTextSearch Tv model
        ]


viewQueryResponse : Model -> Element Msg
viewQueryResponse model =
    column [ width fill ]
        [ text <|
            case model.receivedQuery of
                Nothing ->
                    "No query received"

                Just query ->
                    "Query: " ++ query
        , text <|
            case model.receivedQueryError of
                Nothing ->
                    "No error"

                error ->
                    "Error: " ++ Debug.toString error
        ]


view : Model -> Html.Html Msg
view model =
    Element.layoutWith { options = [] }
        []
    <|
        column [ width fill, spacingXY 0 20 ]
            [ text "TORRENTS"
            , row [ width fill ]
                [ Input.radioRow [ spacing 20, width fill ]
                    { onChange = OnChangeCategory
                    , selected = Just model.category
                    , label = Input.labelAbove [] <| text "Category"
                    , options =
                        [ Input.option Film (text "Film")
                        , Input.option Tv (text "TV")
                        ]
                    }
                ]
            , row [ width fill ]
                [ case model.category of
                    NoCategory ->
                        Element.none

                    Film ->
                        viewFilmOptions model

                    Tv ->
                        viewTvOptions model
                ]
            , row [ width fill ]
                [ if model.text_search /= "" then
                    case model.category of
                        Film ->
                            primary_button [] SubmitFilmSearch "Film Search"

                        Tv ->
                            primary_button [] SubmitTvSearch "TV Search"

                        NoCategory ->
                            Element.none

                  else
                    Element.none
                ]
            , column [ width fill ]
                [ viewQueryResponse model
                ]
            ]


primary_button_custom : List (Element.Attribute Msg) -> Msg -> Element Msg -> Element Msg
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


primary_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
primary_button custom_attrs on_press label =
    primary_button_custom custom_attrs on_press (text label)


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
    hex_to_color "#6c757d"


color_danger : Color
color_danger =
    hex_to_color "#dc3545"


color_primary : Color
color_primary =
    hex_to_color "#007bff"


primary_color_bright : Color
primary_color_bright =
    hex_to_color "#66b0ff"


type alias ButtonConfig =
    { font_color : Color
    , button_color : Color
    , hovered_button_color : Color
    , hovered_font_color : Color
    }


common_button_attrs : ButtonConfig -> List (Element.Attribute Msg)
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
        , Border.color <| primary_color_bright
        , Font.color <| rgb 0 0 0
        ]
    ]
