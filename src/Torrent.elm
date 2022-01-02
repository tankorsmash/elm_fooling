module Torrent exposing (Model, Msg, init, update, view)

import Array
import Browser.Dom
import Browser.Events
import Color
import Color.Convert as Convert
import DOM exposing (offsetWidth, target)
import DateFormat.Relative as Relative
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


type alias QTorrentItem =
    { added_on : Int
    , amount_left : Int
    , auto_tmm : Bool
    , availability : Float
    , category : String
    , completed : Int
    , completion_on : Int
    , content_path : String
    , dl_limit : Int
    , dlspeed : Int
    , downloaded : Int
    , downloaded_session : Int
    , eta : Int
    , f_l_piece_prio : Bool
    , force_start : Bool
    , hash : String
    , last_activity : Int
    , magnet_uri : String
    , max_ratio : Float
    , max_seeding_time : Int
    , name : String
    , num_complete : Int
    , num_incomplete : Int
    , num_leechs : Int
    , num_seeds : Int
    , priority : Int
    , progress : Float
    , ratio : Float
    , ratio_limit : Float
    , save_path : String
    , seeding_time : Int
    , seeding_time_limit : Int
    , seen_complete : Int
    , seq_dl : Bool
    , state : String
    , super_seeding : Bool
    , tags : String
    , time_active : Int
    , total_size : Int
    , tracker : String
    , trackers_count : Int
    , up_limit : Int
    , uploaded : Int
    , uploaded_session : Int
    , upspeed : Int
    }


type alias TorrentNameInfo =
    { resolution : Maybe String
    , quality : Maybe String
    , codec : Maybe String
    , group : Maybe String
    , title : Maybe String
    , year : Maybe Int
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
    , torrentNameInfo : Maybe TorrentNameInfo
    }


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
    , startedSuccessfully : Maybe Bool
    , startedSuccessfullyError : Maybe Http.Error
    , receivedAllTorrentInfo : Maybe (List QTorrentItem)
    , receivedAllTorrentInfoError : Maybe Http.Error
    , now : Time.Posix
    , receivedParsedTorrentName : Maybe (List TorrentNameInfo)
    , receivedParsedTorrentNameError : Maybe Http.Error
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
    | StartDownloadTorrent String
    | ReceivedStartedDownloadingTorrent (Result Http.Error Bool)
    | ReceivedQueryResponse (Result Http.Error String)
    | ReceivedSearchResponse (Result Http.Error (List TorrentItem))
    | StartAllTorrentsInfo
    | ReceivedStartAllTorrentsInfo (Result Http.Error (List QTorrentItem))
    | GotTimeNow Time.Posix
    | ParseTorrentName String
    | ReceivedParseTorrentName (Result Http.Error (List TorrentNameInfo))


type Category
    = NoCategory
    | Film
    | Tv


init : ( Model, Cmd Msg )
init =
    ( { category = NoCategory
      , text_search = ""
      , receivedQuery = Nothing
      , receivedQueryError = Nothing
      , receivedSearch = Nothing
      , receivedSearchError = Nothing
      , tvSeason = Nothing
      , tvEpisode = Nothing
      , tvComplete = False
      , allowUntrustedUsers = False
      , startedSuccessfully = Nothing
      , startedSuccessfullyError = Nothing
      , receivedAllTorrentInfo = Nothing
      , receivedAllTorrentInfoError = Nothing
      , now = Time.millisToPosix 0
      , receivedParsedTorrentName = Nothing
      , receivedParsedTorrentNameError = Nothing
      }
    , Task.perform GotTimeNow Time.now
    )


optionalInt fieldStr =
    optional fieldStr (Decode.nullable Decode.int) Nothing


optionalString fieldStr =
    optional fieldStr (Decode.nullable Decode.string) Nothing


decode_torrent_name_info : Decoder TorrentNameInfo
decode_torrent_name_info =
    Decode.succeed TorrentNameInfo
        |> optionalString "resolution"
        |> optionalString "quality"
        |> optionalString "codec"
        |> optionalString "group"
        |> optionalString "title"
        |> optionalInt "year"


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
        |> optional "torrent_name_info" (Decode.nullable decode_torrent_name_info) Nothing


decode_qtorrent_item : Decoder QTorrentItem
decode_qtorrent_item =
    Decode.succeed QTorrentItem
        |> required "added_on" Decode.int
        |> required "amount_left" Decode.int
        |> required "auto_tmm" Decode.bool
        |> required "availability" Decode.float
        |> required "category" Decode.string
        |> required "completed" Decode.int
        |> required "completion_on" Decode.int
        |> required "content_path" Decode.string
        |> required "dl_limit" Decode.int
        |> required "dlspeed" Decode.int
        |> required "downloaded" Decode.int
        |> required "downloaded_session" Decode.int
        |> required "eta" Decode.int
        |> required "f_l_piece_prio" Decode.bool
        |> required "force_start" Decode.bool
        |> required "hash" Decode.string
        |> required "last_activity" Decode.int
        |> required "magnet_uri" Decode.string
        |> required "max_ratio" Decode.float
        |> required "max_seeding_time" Decode.int
        |> required "name" Decode.string
        |> required "num_complete" Decode.int
        |> required "num_incomplete" Decode.int
        |> required "num_leechs" Decode.int
        |> required "num_seeds" Decode.int
        |> required "priority" Decode.int
        |> required "progress" Decode.float
        |> required "ratio" Decode.float
        |> required "ratio_limit" Decode.float
        |> required "save_path" Decode.string
        |> required "seeding_time" Decode.int
        |> required "seeding_time_limit" Decode.int
        |> required "seen_complete" Decode.int
        |> required "seq_dl" Decode.bool
        |> required "state" Decode.string
        |> required "super_seeding" Decode.bool
        |> required "tags" Decode.string
        |> required "time_active" Decode.int
        |> required "total_size" Decode.int
        |> required "tracker" Decode.string
        |> required "trackers_count" Decode.int
        |> required "up_limit" Decode.int
        |> required "uploaded" Decode.int
        |> required "uploaded_session" Decode.int
        |> required "upspeed" Decode.int


clearQueryAndSearchResults : Model -> Model
clearQueryAndSearchResults model =
    { model
        | receivedQuery = Nothing
        , receivedQueryError = Nothing
        , receivedSearch = Nothing
        , receivedSearchError = Nothing
    }



-- url_root = "http://0.0.0.0:4126"
-- url_root = "http://localhost:4126"


url_root : String
url_root =
    "http://192.168.2.41:4126"


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
                        ( { m | allowUntrustedUsers = newAllowUntrustedUsers }, Cmd.none )
                   )

        SubmitFilmSearch ->
            let
                http_request =
                    Http.post
                        { url = url_root ++ "/torrent/search"
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
                http_request : Cmd Msg
                http_request =
                    Http.post
                        { url = url_root ++ "/torrent/search"
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

        StartDownloadTorrent link ->
            ( model
            , Http.post
                { url = url_root ++ "/torrent/download"
                , body =
                    Encode.object
                        [ ( "link", Encode.string link )
                        , ( "category"
                          , case model.category of
                                Film ->
                                    Encode.string "Movies"

                                Tv ->
                                    Encode.string "TV"

                                NoCategory ->
                                    Encode.string "Movies"
                          )
                        ]
                        |> Http.jsonBody
                , expect =
                    Http.expectJson ReceivedStartedDownloadingTorrent
                        (field "success" Decode.bool)
                }
            )

        ReceivedStartedDownloadingTorrent started_resp ->
            case started_resp of
                Ok success ->
                    ( { model
                        | startedSuccessfully = Just success
                        , startedSuccessfullyError = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | startedSuccessfully = Nothing
                        , startedSuccessfullyError = Just error
                      }
                    , Cmd.none
                    )

        StartAllTorrentsInfo ->
            ( model
            , Http.post
                { url = url_root ++ "/torrent/info"
                , body = Encode.object [] |> Http.jsonBody
                , expect =
                    Http.expectJson ReceivedStartAllTorrentsInfo
                        (field "response" (field "torrents" (Decode.list decode_qtorrent_item)))
                }
            )

        ReceivedStartAllTorrentsInfo started_resp ->
            case started_resp of
                Ok success ->
                    ( { model
                        | receivedAllTorrentInfo = Just success
                        , receivedAllTorrentInfoError = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | receivedAllTorrentInfo = Nothing
                        , receivedAllTorrentInfoError = Just error
                      }
                    , Cmd.none
                    )

        GotTimeNow new_now ->
            ( { model | now = new_now }, Cmd.none )

        ParseTorrentName torrent_name ->
            ( model
            , Http.post
                { url = url_root ++ "/torrent/parse"
                , body =
                    Encode.object
                        [ ( "filename"
                          , Encode.string torrent_name
                          )
                        ]
                        |> Http.jsonBody
                , expect =
                    Http.expectJson ReceivedParseTorrentName
                        (field "response" (field "torrent_names" (Decode.list decode_torrent_name_info)))
                }
            )

        ReceivedParseTorrentName started_resp ->
            case started_resp of
                Ok success ->
                    ( { model
                        | receivedParsedTorrentName = Just success
                        , receivedParsedTorrentNameError = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | receivedParsedTorrentName = Nothing
                        , receivedParsedTorrentNameError = Just error
                      }
                    , Cmd.none
                    )


explain : Element.Attribute msg
explain =
    Element.explain Debug.todo


viewTextSearch : Category -> Model -> Element Msg
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
                            "Movie"

                        Tv ->
                            "Show name"

                        NoCategory ->
                            "Search:"
        }


viewFilmOptions : Model -> Element Msg
viewFilmOptions model =
    row [ width fill ]
        [ viewTextSearch Film model
        ]


viewTvOptions : Model -> Element Msg
viewTvOptions model =
    row [ width fill ]
        [ el [ width <| fillPortion 5 ] <| viewTextSearch Tv model
        , Input.text [ alignBottom ]
            { onChange = String.toInt >> OnChangeTvSeason
            , text = Maybe.withDefault "" (Maybe.map String.fromInt model.tvSeason)
            , placeholder = Just <| Input.placeholder [] <| text "123"
            , label = Input.labelAbove [] <| Element.el [ Font.size 20 ] <| text "Season"
            }
        , el [ alignBottom ] <|
            Input.text []
                { onChange = String.toInt >> OnChangeTvEpisode
                , text = Maybe.withDefault "" (Maybe.map String.fromInt model.tvEpisode)
                , placeholder = Just <| Input.placeholder [] <| text "321"
                , label = Input.labelAbove [] <| Element.el [ Font.size 20 ] <| text "Episode"
                }
        ]


monospace attrs el =
    Element.el (Font.family [ Font.monospace ] :: attrs) el


renderTorrentInfo : TorrentNameInfo -> Element Msg
renderTorrentInfo nameInfo =
    let
        title =
            Element.el [ Font.size 24 ] <| text (nameInfo.title |> Maybe.withDefault "???" |> (\t -> clipText t 50))

        year =
            case nameInfo.year of
                Nothing ->
                    text ""

                Just year_ ->
                    text <| "(" ++ String.fromInt year_ ++ ")"

        resolution =
            case nameInfo.resolution of
                Nothing ->
                    text ""

                Just resolution_ ->
                    text resolution_

        codec =
            case nameInfo.codec of
                Nothing ->
                    text ""

                Just codec_ ->
                    text codec_

        quality =
            case nameInfo.quality of
                Nothing ->
                    text ""

                Just quality_ ->
                    text quality_
    in
    paragraph []
        [ title
        , year
        , text " - "
        , monospace [ Font.size 14 ] resolution
        , text " | "
        , monospace [ Font.size 14 ] quality
        , text " "
        , monospace [ Font.size 14 ] codec
        ]


torrentItemTableConfig : List (Element.Column TorrentItem Msg)
torrentItemTableConfig =
    [ { header = text "Name"
      , width = fillPortion 5
      , view =
            \item ->
                case item.torrentNameInfo of
                    Just nameInfo ->
                        renderTorrentInfo nameInfo

                    Nothing ->
                        text (clipText item.name 50)
      }
    , { header = text "Uploader"
      , width = fillPortion 1
      , view = \item -> el [ Font.size 14 ] <| text item.uploader
      }
    , { header = text "Download"
      , width = fillPortion 2
      , view = \item -> primary_button [] (StartDownloadTorrent item.link) "Download"
      }
    ]


clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


float_to_percent : Float -> String
float_to_percent flt =
    flt * 100 |> floor |> String.fromInt |> (\str -> str ++ "%")


etaToString : Int -> String
etaToString eta =
    if eta == 8640000 then
        ""

    else if eta < 60 * 5 then
        String.fromInt eta ++ "sec"

    else if eta < 60 * 60 then
        String.fromInt (eta * 60) ++ "min"

    else if eta < 60 * 60 * 24 then
        String.fromInt (eta * 60 * 60) ++ "hr"

    else
        String.fromInt (eta * 60 * 60 * 24) ++ "d"


qTorrentsListColumnConfig : Time.Posix -> List (Element.Column QTorrentItem Msg)
qTorrentsListColumnConfig now =
    [ { header = text "Name"
      , width = fillPortion 1
      , view =
            \item ->
                Element.el
                    [ Element.scrollbarX
                    , Element.clipY
                    , width (fillPortion 4 |> Element.maximum 700)
                    , centerY
                    ]
                <|
                    text (clipText item.name 30)
      }
    , { header = Element.el [ Font.center ] <| text "Progress"
      , width = fillPortion 1
      , view =
            \item ->
                Element.el
                    [ width fill
                    , Font.center
                    ]
                <|
                    text <|
                        float_to_percent item.progress
      }
    , { header = Element.el [ Font.center ] <| text "ETA"
      , width = fillPortion 1
      , view =
            \item ->
                Element.el
                    [ width fill
                    , Font.center
                    ]
                <|
                    text <|
                        etaToString item.eta
      }
    , { header = Element.el [ Font.center ] <| text "State"
      , width = fillPortion 1
      , view =
            \item ->
                Element.el
                    [ width fill
                    , Font.center
                    ]
                <|
                    text item.state
      }
    , { header = Element.el [ Font.center ] <| text "Added on"
      , width = fillPortion 1
      , view =
            \item ->
                Element.el
                    [ width fill
                    , Font.center
                    ]
                <|
                    text <|
                        let
                            rawAddedOn =
                                round <| toFloat item.added_on * 1000

                            added_on =
                                Time.millisToPosix rawAddedOn
                        in
                        Relative.relativeTime now added_on
      }
    ]


nameRenderer : TorrentNameInfo -> Element Msg
nameRenderer nameInfo =
    column [ Font.family [ Font.monospace ] ] <|
        [ text <| "res: " ++ (nameInfo.resolution |> Maybe.withDefault "???")
        , text <| "qua: " ++ (nameInfo.quality |> Maybe.withDefault "???")
        , text <| "cod: " ++ (nameInfo.codec |> Maybe.withDefault "???")
        , text <| "grp: " ++ (nameInfo.group |> Maybe.withDefault "???")
        , text <| "tit: " ++ (nameInfo.title |> Maybe.withDefault "???")
        , text <|
            "yr:  "
                ++ (nameInfo.year
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault "--"
                   )
        ]


viewTorrentNameInfo : Model -> Element Msg
viewTorrentNameInfo model =
    let
        renderedInfos =
            case model.receivedParsedTorrentName of
                Just name_infos ->
                    List.map nameRenderer name_infos

                Nothing ->
                    [ case model.receivedParsedTorrentNameError of
                        Just error ->
                            Element.el [ Font.color <| rgb 1 0 0 ] <| text <| Debug.toString error

                        Nothing ->
                            Element.none
                    ]

        header =
            row []
                [ text "Torrent name info"
                , primary_button [] (ParseTorrentName "Antlers.2021.1080p.WEBRip.x265-RARBG") "Get name"
                ]
    in
    column [ width fill ] <|
        [ header
        ]
            ++ renderedInfos


viewQTorrents : Model -> Element Msg
viewQTorrents model =
    let
        _ =
            123
    in
    column [ width fill ]
        [ primary_button [] StartAllTorrentsInfo "Update"
        , case model.receivedAllTorrentInfo of
            Just torrents_info ->
                Element.table
                    [ width fill
                    , Font.size 24
                    , spacingXY 10 10
                    ]
                    { data = List.reverse <| List.sortBy .added_on torrents_info
                    , columns = qTorrentsListColumnConfig model.now
                    }

            Nothing ->
                case model.receivedAllTorrentInfoError of
                    Just error ->
                        Element.el [ Font.color <| rgb 1 0 0 ] <| text <| Debug.toString error

                    Nothing ->
                        Element.none
        ]


viewSearchResponse : Model -> Element Msg
viewSearchResponse model =
    column [ width fill ]
        [ case model.receivedSearch of
            Nothing ->
                text "Please choose an option above"

            Just items ->
                case items of
                    [] ->
                        if model.allowUntrustedUsers then
                            text "No results found. Maybe allow sketchy users?"

                        else
                            text "No results found. Is there a typo?"

                    _ ->
                        Element.table [ width fill, Font.size 16, spacingXY 0 10 ]
                            { data = items
                            , columns = torrentItemTableConfig
                            }
        , Element.el [ Font.color <| rgb 1 0 0 ] <|
            case model.receivedSearchError of
                Nothing ->
                    Element.none

                error ->
                    let
                        _ =
                            Debug.log "search response error" error
                    in
                    text <| "Search error: " ++ Debug.toString error
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
        , case model.receivedQueryError of
            Nothing ->
                Element.none

            error ->
                let
                    _ =
                        Debug.log "search response error" error
                in
                text <| "Search error: " ++ Debug.toString error
        ]


categoryRadioOption : String -> Input.OptionState -> Element msg
categoryRadioOption str option_state =
    case option_state of
        Input.Idle ->
            el [ Font.color color_secondary ] <| text <| "Search " ++ str

        Input.Focused ->
            el [ Font.color color_danger ] <| text <| "Search " ++ str

        Input.Selected ->
            el [ Font.color color_primary ] <| text <| "Search " ++ str


view : Model -> Html.Html Msg
view model =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
    <|
        column [ width fill, spacingXY 0 20, Font.size 48 ]
            [ text "TORRENTS"
            , row [ width fill ]
                [ Input.radioRow [ spacing 20, width fill ]
                    { onChange = OnChangeCategory
                    , selected = Just model.category
                    , label = Input.labelAbove [] <| el [ Font.size 24 ] <| text "Category"
                    , options =
                        [ Input.optionWith Film (categoryRadioOption "Film")
                        , Input.optionWith Tv (categoryRadioOption "TV")
                        ]
                    }
                ]
            , row [ width fill ]
                [ Input.checkbox [ width fill ]
                    { onChange = OnChangeAllowUntrustedUsers
                    , icon =
                        \checked ->
                            if checked then
                                el [ Font.color <| rgb255 170 108 57 ] <| text "Allowing everyone"

                            else
                                text "Only allowing trusted uploaders"
                    , checked = model.allowUntrustedUsers
                    , label = Input.labelAbove [] <| el [ Font.size 24 ] <| text "Allow Sketchy Users?"
                    }
                ]
            , row [ width fill ]
                [ case model.startedSuccessfully of
                    Just success ->
                        if success then
                            el [ Font.color <| rgb 0 1 0 ] <| text "Started download successfully!"

                        else
                            text "Failed to start for some reason"

                    Nothing ->
                        Element.none
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
                            primary_button [ width fill, height fill ] SubmitFilmSearch "Film Search"

                        Tv ->
                            primary_button [ width fill, height fill ] SubmitTvSearch "TV Search"

                        NoCategory ->
                            Element.none

                  else
                    Element.none
                ]
            , column [ width fill ] [ viewSearchResponse model ]
            , row
                [ width fill
                , paddingXY 0 15
                , Border.width 2
                , Border.dotted
                , Border.widthEach { top = 4, bottom = 0, left = 0, right = 0 }
                ]
                [ text "CURRENT TORRENTS" ]
            , column [ width fill ] [ viewQTorrents model ]

            -- , column [ width fill ] [ viewTorrentNameInfo model ]
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
