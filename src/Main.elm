module Main exposing (..)

import String 
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Browser
import Browser.Navigation as Nav
import Debug
import Html
    exposing
        ( Html
        , a
        , b
        , br
        , button
        , div
        , h1
        , h2
        , h3
        , h4
        , input
        , p
        , span
        , table
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (attribute, href, property, style)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, at, field, list, string)
import Json.Encode exposing (string)
import List
import PostData exposing (PostData)
import Table exposing (ColumnDef, ColumnLookup, ColumnType, TableDefinition, view)
import Task
import Time
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)

import Reddit


type Msg
    = Increment
    | Decrement
    | Poop
    | Change String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DownloadAllPosts
    | DownloadedAllPosts (Result Http.Error (List PostData))
    | GotPostById (Result Http.Error PostData)
    | PostIDToDownloadChanged String
    | DownloadPostById Int
    | AlertModalShow String
    | AlertModalHide

    | DownloadRedditPosts
    | DownloadedRedditPosts (Result Http.Error Reddit.Listing)



-- MAIN


root_json_server_url =
    "http://localhost:5021/"


add_class cls =
    property "className" (Json.Encode.string cls)


bootstrap_button type_ on_click text_ =
    Button.button
        [ type_
        , Button.attrs [ onClick on_click ]
        ]
        [ text text_ ]


button_primary on_click text_ =
    bootstrap_button Button.primary on_click text_


button_secondary on_click text_ =
    bootstrap_button Button.secondary on_click text_


empty_div : Html msg
empty_div =
    div [] []


main =
    -- Browser.element {
    --     init = init, view = view, update = update2,
    --     subscriptions = subscriptions
    -- }
    Browser.application
        { init = init
        , view = view
        , update = update2
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        -- , DownloadedRedditPosts
        ]



-- MODEL


type alias PageInfo =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , page : Page
    }

-- type alias RedditSubmission


type alias Model =
    { count : Int
    , content : String
    , time : Time.Posix
    , zone : Time.Zone
    , page_info : PageInfo
    , post_data : PostData
    , post_datas : List PostData
    , post_id_to_download : Int
    , post_id_to_download_err_status : Int
    , alert_modal_open : Modal.Visibility
    , alert_modal_text : String
    , reddit_listing : Reddit.Listing
    , reddit_is_downloaded : Bool
    }


type Page
    = NotFoundPage
    | HomePage
    | ProfilePage
    | UnsetPage


type Route
    = Home
    | NotFound
    | Profile
      -- | Topic String
      -- | Blog Int
      -- | User String
      -- | Comment String Int
    | UnsetRoute


parseUrl : Url.Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Home (s "home")
        , map Profile (s "profile")

        -- , map Topic   (s "topic" </> string)
        -- , map Blog    (s "blog" </> int)
        -- , map User    (s "user" </> string)
        -- , map Comment (s "user" </> string </> s "comment" </> int)
        ]


download_post_by_id : Int -> Cmd Msg
download_post_by_id post_id =
    Http.get
        { url = root_json_server_url ++ "posts/" ++ String.fromInt post_id
        , expect = Http.expectJson GotPostById PostData.decode_single
        }


download_all_posts : Cmd Msg
download_all_posts =
    Http.get
        { url = root_json_server_url ++ "posts"
        , expect = Http.expectJson DownloadedAllPosts (list PostData.decode_single)
        }



decode_post_titles : Decoder (List String)
decode_post_titles =
    list (field "title" Json.Decode.string)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        parsedRoute =
            parseUrl url

        page_info =
            PageInfo navKey url (parseUrl url) UnsetPage

        post_data =
            PostData -1 "No Name" "No Title"

        reddit_listing = Reddit.Listing "" "" []

        model =
            { count = 0
            , content = "ASD"
            , time = Time.millisToPosix 0
            , zone = Time.utc
            , page_info = page_info
            , post_data = post_data
            , post_datas = []
            , post_id_to_download = -1
            , post_id_to_download_err_status = 0
            , alert_modal_open = Modal.hidden
            , alert_modal_text = ""
            , reddit_listing = reddit_listing
            , reddit_is_downloaded = False
            }

        existingCmds =
            Task.perform AdjustTimeZone Time.here
    in
    initCurrentPage ( model, existingCmds )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.page_info.route of
                Home ->
                    ( HomePage, Cmd.none )

                Profile ->
                    ( ProfilePage, Cmd.none )

                NotFound ->
                    ( NotFoundPage, Cmd.none )

                UnsetRoute ->
                    ( NotFoundPage, Cmd.none )

        page_info =
            model.page_info
    in
    ( { model | page_info = { page_info | page = currentPage } }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )



-- UPDATE


update2 : Msg -> Model -> ( Model, Cmd Msg )
update2 msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 10 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        Poop ->
            ( { model | count = model.count + 2 }, Cmd.none )

        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.page_info.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                page_info =
                    model.page_info

                newRoute =
                    parseUrl url
            in
            ( { model | page_info = { page_info | url = url, route = newRoute } }
            , Cmd.none
            )
                |> initCurrentPage

        DownloadAllPosts ->
            ( model, download_all_posts )

        DownloadedAllPosts result ->
            case result of
                Ok new_post_datas ->
                    let
                        post_datas =
                            Debug.log "Successfully received files!" new_post_datas
                    in
                    ( { model | post_datas = post_datas }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody err_msg ->
                            ( Debug.log err_msg model, Cmd.none )

                        _ ->
                            ( Debug.log "Unknown error downloading" model, Cmd.none )

        GotPostById result ->
            case result of
                Ok new_post_data ->
                    let
                        existing_post_data =
                            Debug.log "Successfully received files!" model.post_data
                    in
                    ( { model | post_data = new_post_data, post_id_to_download_err_status = 0 }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody err_msg ->
                            ( Debug.log err_msg model, Cmd.none )

                        Http.BadStatus status ->
                            ( Debug.log
                                ("Received a bad status: " ++ String.fromInt status)
                                { model | post_id_to_download_err_status = status }
                            , Cmd.none
                            )

                        _ ->
                            ( Debug.log "Unknown error downloading" model, Cmd.none )

        PostIDToDownloadChanged strPostId ->
            let
                maybePostId =
                    String.toInt strPostId
            in
            case maybePostId of
                Just intPostId ->
                    ( { model | post_id_to_download = intPostId }, Cmd.none )

                Nothing ->
                    Debug.log "couldnt convert to int" ( model, Cmd.none )

        DownloadPostById post_id_to_download_ ->
            ( model, download_post_by_id post_id_to_download_ )

        AlertModalShow alert_text ->
            ( { model | alert_modal_open = Modal.shown, alert_modal_text = alert_text }, Cmd.none )

        AlertModalHide ->
            ( { model | alert_modal_open = Modal.hidden }, Cmd.none )

        DownloadRedditPosts ->
            ( model, (Reddit.custom_download_reddit_posts DownloadedRedditPosts) )
            -- ( model, Reddit.download_reddit_posts )

        DownloadedRedditPosts result ->
            case result of
                Ok new_listing ->
                    let
                        lg = Debug.log "Successfully received listing!" "DDDD"
                        listing =
                             new_listing
                    in
                    ( { model | reddit_listing = listing }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody err_msg ->
                            let
                                lg = Debug.log "Error message handling the reddit post" "."
                                trimmed = (String.slice 0 250 (err_msg)) ++ "... trimmed"
                                lg2 = Debug.log trimmed "."
                            in
                            (  model, Cmd.none )

                        _ ->
                            ( Debug.log "Unknown error downloading" model, Cmd.none )


humanize : Time.Posix -> Time.Zone -> String
humanize time zone =
    let
        hour =
            String.fromInt (Time.toHour zone time)

        minute =
            String.fromInt (Time.toMinute zone time)

        second =
            String.fromInt (Time.toSecond zone time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second



-- VIEW


navigation : Model -> Html Msg
navigation model =
    div [ style "margin-bottom" "15px" ]
        [ div []
            [ text "This is a link: "
            , a [ href "/home" ] [ text "HOME" ]
            ]
        , div []
            [ text "This is my profile: "
            , a [ href "/profile" ] [ text "PROFILE" ]
            ]
        ]


my_column_defs : List ColumnDef
my_column_defs =
    [ { column_id = "title"
      , idx = 2
      , pretty_title = "The Title"
      }
    , { column_id = "author"
      , idx = 1
      , pretty_title = "Author"
      }
    , { column_id = "post_id"
      , idx = 0
      , pretty_title = "ID"
      }
    ]


my_column_lookups : List (ColumnLookup PostData)
my_column_lookups =
    let
        title =
            ColumnLookup "title" .title
        id_ =
            ColumnLookup "post_id" (\o -> String.fromInt o.id)
        author =
            ColumnLookup "author" .author
    in
    [ title, author, id_ ]


my_row_datas : List PostData
my_row_datas =
    [ PostData 123 "The End Times" "Matthew"
    , PostData 51 "Everything Ends" "Joshua"
    , PostData 3 "The Pale Horse" "Olivia"
    ]


my_table_definition : TableDefinition
my_table_definition =
    { columns = my_column_defs }



--call a list of functions on an row


do_lookups : List (obj -> String) -> obj -> List String
do_lookups lookups row =
    List.foldl (\func acc -> acc ++ [ func row ]) [] lookups


homeView : Model -> Html Msg
homeView model =
    let
        columns =
            my_table_definition.columns

        -- single_col = case (List.head columns) of
        --     Nothing -> { column_loop = "" }
        --     Just col -> col
        lookups =
            -- List.map .column_lookup columns
            -- List.map .column_lookup my_column_lookups
            List.map .lookup_func my_column_lookups

        table_rows : List (List String)
        table_rows =
            List.map (do_lookups lookups) model.post_datas
    in
    div [ add_class "container" ]
        [ navigation model
        , CDN.stylesheet
        , h3 [] [ text "HOME PAGE IS HERE!!!" ]
        , div []
            [ text <| "Post data -- Title: " ++ model.post_data.title ++ ", and Author: " ++ model.post_data.author
            ]
        , button_primary DownloadAllPosts "Download All Posts"
        , br [] []
        , button_primary DownloadRedditPosts "Download Reddit Data"
        , br [] []
        , div []
            [ Grid.row []
                [ Grid.col [ Col.lg3 ]
                    [ InputGroup.config
                        (InputGroup.number
                            [ Input.placeholder "post_id"
                            , Input.value (String.fromInt model.post_id_to_download)
                            , Input.onInput PostIDToDownloadChanged
                            ]
                        )
                        |> InputGroup.predecessors
                            [ InputGroup.span [] [ text "Post ID" ] ]
                        |> InputGroup.view
                    ]
                , Grid.col [ Col.lg6 ]
                    [ button_secondary (DownloadPostById model.post_id_to_download) "Download Entire PostData" ]
                ]
            , case model.post_id_to_download_err_status of
                0 ->
                    empty_div

                _ ->
                    div [ style "color" "red" ] [ text <| "ERROR STATUS: " ++ String.fromInt model.post_id_to_download_err_status ]
            , Grid.row []
                [ Grid.col [ Col.sm3 ]
                    [ text "left" ]
                , Grid.col [ Col.sm9 ]
                    [ text "right" ]
                ]
            ]
        , br [] []
        , Table.view my_table_definition table_rows
        , br [] []
        , div []
            [ Button.button
                [ Button.primary
                , Button.attrs [ onClick <| AlertModalShow "This is text" ]
                ]
                [ text "Show Modal" ]
            , Modal.config AlertModalHide
                |> Modal.small
                |> Modal.hideOnBackdropClick True
                |> Modal.h3 [] [ text "Modal Header" ]
                |> Modal.body [] [ p [] [ text model.alert_modal_text ] ]
                |> Modal.footer []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ onClick AlertModalHide ]
                        ]
                        [ text "Close" ]
                    ]
                |> Modal.view model.alert_modal_open
            ]
        ]



-- turns ['a', 'b', 'c'] -> [(0, 'a'), (1, 'b'), (2, 'c')]


join_with_numbers : List a -> List ( Int, a )
join_with_numbers to_join =
    List.map2 Tuple.pair (List.range 0 (List.length to_join)) to_join


profileView : Model -> Html Msg
profileView model =
    div []
        [ navigation model
        , text "Welcome to my Profile!!"
        ]



-- view : Model -> Html Msg

view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ case model.page_info.page of
            NotFoundPage ->
                div [] [ text "Not found page" ]

            HomePage ->
                homeView model

            ProfilePage ->
                profileView model

            UnsetPage ->
                div [] [ text "UNSET PAGE" ]
        ]
    }
