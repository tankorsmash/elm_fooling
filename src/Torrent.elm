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
    , receivedSearch : Maybe (List TorrentItem)
    , receivedSearchError : Maybe Http.Error
    , tvSeason : Maybe Int
    , tvEpisode : Maybe Int
    , tvComplete : Bool
    , allowUntrustedUsers : Bool
    }


type Msg
    = OnChangeCategory Category
    | OnChangeTextSearch String
    | OnChangeTvSeason (Maybe Int)
    | OnChangeTvEpisode (Maybe Int)
    | OnChangeTvComplete Bool
    | OnChangeAllowUntrustedUsers Bool
    | SubmitFilmSearch
    | SubmitTvSearch
    | ReceivedQueryResponse (Result Http.Error String)
    | ReceivedSearchResponse (Result Http.Error (List TorrentItem))


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
    , receivedSearch = Nothing
    , receivedSearchError = Nothing
    , tvSeason = Nothing
    , tvEpisode = Nothing
    , tvComplete = False
    , allowUntrustedUsers = False
    }


type alias TorrentItem =
    { name : String
    , torrentId : String
    , link : String
    , seeders : String
    , leechers : String
    , size : String
    , time : String
    , uploader : String
    , uploaderLink : String
    }


decode_torrent_item : Decoder TorrentItem
decode_torrent_item =
    Decode.succeed TorrentItem
        |> required "name" Decode.string
        |> required "torrentId" Decode.string
        |> required "link" Decode.string
        |> required "seeders" Decode.string
        |> required "leechers" Decode.string
        |> required "size" Decode.string
        |> required "time" Decode.string
        |> required "uploader" Decode.string
        |> required "uploaderLink" Decode.string


clearQueryAndSearchResults : Model -> Model
clearQueryAndSearchResults model =
    { model
        | receivedQuery = Nothing
        , receivedQueryError = Nothing
        , receivedSearch = Nothing
        , receivedSearchError = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnChangeCategory new_category ->
            model
                |> clearQueryAndSearchResults
                |> (\m ->
                        ( { m | category = new_category }, Cmd.none )
                   )

        OnChangeTextSearch new_text_search ->
            model
                |> clearQueryAndSearchResults
                |> (\m ->
                        ( { m | text_search = new_text_search }, Cmd.none )
                   )

        OnChangeTvSeason newSeason ->
            model
                |> clearQueryAndSearchResults
                |> (\m ->
                        ( { m | tvSeason = newSeason }, Cmd.none )
                   )

        OnChangeTvEpisode newEpisode ->
            model
                |> clearQueryAndSearchResults
                |> (\m ->
                        ( { m | tvEpisode = newEpisode }, Cmd.none )
                   )

        OnChangeTvComplete newComplete ->
            model
                |> clearQueryAndSearchResults
                |> (\m ->
                        ( { m | tvComplete = newComplete }, Cmd.none )
                   )
        OnChangeAllowUntrustedUsers newAllowUntrustedUsers ->
            model
                |> clearQueryAndSearchResults
                |> (\m ->
                        ( { m | allowUntrustedUsers = newAllowUntrustedUsers}, Cmd.none )
                   )

        SubmitFilmSearch ->
            let
                http_request =
                    Http.post
                        { url = "http://localhost:4126/torrent/search"
                        , body =
                            Encode.object
                                [ ( "query", Encode.string model.text_search )
                                , ( "category", Encode.string "movies" )
                                , ( "allow_untrusted_users", Encode.bool model.allowUntrustedUsers )
                                ]
                                |> Http.jsonBody
                        , expect =
                            Http.expectJson ReceivedSearchResponse
                                (field "response" (field "items" <| Decode.list decode_torrent_item))
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
                                [ ( "query", Encode.string model.text_search )
                                , ( "category", Encode.string "TV" )
                                , ( "season"
                                  , case model.tvSeason of
                                        Just season ->
                                            Encode.int season

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "episode"
                                  , case model.tvEpisode of
                                        Just episode ->
                                            Encode.int episode

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "allow_untrusted_users", Encode.bool model.allowUntrustedUsers )

                                -- , ( "complete", Encode.bool model.tvComplete )
                                ]
                                |> Http.jsonBody
                        , expect =
                            Http.expectJson ReceivedSearchResponse
                                (field "response" (field "items" <| Decode.list decode_torrent_item))
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

        ReceivedSearchResponse query_resp ->
            case query_resp of
                Ok query ->
                    ( { model
                        | receivedSearchError = Nothing
                        , receivedSearch = Just query
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | receivedSearchError = Just err
                        , receivedSearch = Nothing
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
            Input.labelAbove [] <|
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


viewTvOptions : Model -> Element Msg
viewTvOptions model =
    row [ width fill ]
        [ el [ width <| fillPortion 3 ] <| viewTextSearch Tv model
        , Input.text []
            { onChange = String.toInt >> OnChangeTvSeason
            , text = Maybe.withDefault "" (Maybe.map String.fromInt model.tvSeason)
            , placeholder = Just <| Input.placeholder [] <| text "Season Number"
            , label = Input.labelAbove [] <| text "Season"
            }
        , Input.text []
            { onChange = String.toInt >> OnChangeTvEpisode
            , text = Maybe.withDefault "" (Maybe.map String.fromInt model.tvEpisode)
            , placeholder = Just <| Input.placeholder [] <| text "Episode Number"
            , label = Input.labelAbove [] <| text "Episode"
            }
        ]


renderTorrentItem : TorrentItem -> Element Msg
renderTorrentItem item =
    text item.name


torrentItemTableConfig : List (Element.Column TorrentItem Msg)
torrentItemTableConfig =
    [ { header = text "Name"
      , width = fillPortion 1
      , view = .name >> (\name -> clipText name 50) >> text
      }
    , { header = text "Uploader"
      , width = fillPortion 1
      , view = .uploader >> text
      }
    ]


clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


viewSearchResponse : Model -> Element Msg
viewSearchResponse model =
    column [ width fill ]
        [ case model.receivedSearch of
            Nothing ->
                text "No search received"

            Just items ->
                -- column []
                --     <| ( text <| "Search results length: " ++ (String.fromInt <| List.length items))
                --     :: List.map renderTorrentItem items
                Element.table [ width fill ]
                    { data = items
                    , columns = torrentItemTableConfig
                    }
        , text <|
            case model.receivedSearchError of
                Nothing ->
                    "No error"

                error ->
                    let
                        _ =
                            Debug.log "search response error" error
                    in
                    "Search error: " ++ Debug.toString error
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
                , Input.checkbox [ spacing 20, width fill ]
                    { onChange = OnChangeAllowUntrustedUsers
                    , icon = Input.defaultCheckbox
                    , checked = model.allowUntrustedUsers
                    , label = Input.labelAbove [] <| text "Allow Sketchy Users"
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
                -- [ viewQueryResponse model
                [ viewSearchResponse model
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
