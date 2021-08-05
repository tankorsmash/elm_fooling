port module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Nav
import Debug
import FormData
    exposing
        ( DataType(..)
        , new_form_field_float
        , new_form_field_int
        , new_form_field_string
        )
import Html
    exposing
        ( Html
        , a
        , b
        , br
        , button
        , div
        , form
        , h1
        , h2
        , h3
        , h4
        , img
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
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, at, field, list, string)
import Json.Encode exposing (string)
import List
import OpenDota.OpenDota as OpenDota
import PostData exposing (PostData)
import Reddit
import String
import Table exposing (ColumnDef, ColumnLookup, ColumnType, TableDefinition, view)
import Task
import Time
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)
import Utils exposing (add_class)
import Weather


type alias WeaponFrame =
    { weapon_name : String, frame_id : Int, choice_id : Int }


type TableType
    = RedditListingTable
    | PostDatasTable


type alias DotaModel =
    { account_id : Int
    , player_data : Maybe OpenDota.PlayerData
    , hero_stats : Maybe (List OpenDota.HeroStat)
    }


type DotaMsg
    = ChangeAccountId Int


type Msg
    = OnPageLoad Time.Posix
    | Increment
    | Decrement
    | Poop
    | Change String
    | Tick Time.Posix
      -- | AdjustTimeZone Time.Zone
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
    | DownloadedRedditPosts (Result Http.Error Reddit.ListingWrapper)
      -- | DownloadedRedditPostsJSONP Reddit.ListingWrapper
    | DownloadedRedditPostsJSONP Json.Decode.Value
    | ChangeTab TabType
    | NavbarMsg Navbar.State
    | DownloadCurrentWeather
    | DownloadCurrentAreasWeather
    | ChangeCurrentWeatherArea String
    | DownloadedCurrentWeather (Result Http.Error Weather.CurrentWeatherResponse)
    | PrevPageMsg TableType
    | NextPageMsg TableType
    | ChangePageMsg TableType Int
    | ChangeSubredditToDownload String
    | SendToPort String
    | RequestJSONP String
    | RequestJSONPFromSubreddit String
    | RecvFromPort String
    | UpdateFormData FormUpdateType
    | SubmitFormData
    | DotaDownloadPlayerData Int
    | DotaDownloadedPlayerData (Result Http.Error OpenDota.PlayerData)
    | DotaDownloadHeroStats
    | DotaDownloadedHeroStats (Result Http.Error (List OpenDota.HeroStat))
    | DotaUpdate DotaMsg



-- MAIN


update_form_data : WeaponFrame -> FormUpdateType -> WeaponFrame
update_form_data form_data form_update_type =
    case form_update_type of
        Name new_name ->
            { form_data | weapon_name = new_name }

        ChoiceId new_choice_id ->
            { form_data
                | choice_id =
                    case String.toInt new_choice_id of
                        Just new_int ->
                            new_int

                        Nothing ->
                            form_data.choice_id
            }

        FrameId new_frame_id ->
            { form_data
                | frame_id =
                    case String.toInt new_frame_id of
                        Just new_int ->
                            new_int

                        Nothing ->
                            form_data.frame_id
            }



-- form_data


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
    --     init = init, view = view, update = update,
    --     subscriptions = subscriptions
    -- }
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ --Time.every 1000 Tick,
          Navbar.subscriptions model.current_navbar_state NavbarMsg
        , test_port_receiving RecvFromPort
        , recv_reddit_listing DownloadedRedditPostsJSONP
        ]



-- MODEL


type alias PageInfo =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , page : Page
    }


type TabType
    = HomeTab
    | SinglePostDataTab
    | PostDataTableTab
    | RedditListingTab
    | WeatherTab
    | FormDataTab
    | ModalTab
    | OpenDotaTab


type FormUpdateType
    = Name String
    | FrameId String
    | ChoiceId String


type alias Model =
    { count : Int
    , content : String
    , time : Time.Posix
    , zone : Time.Zone
    , page_info : PageInfo
    , post_data : PostData
    , post_datas : List PostData
    , post_datas_page_info : Table.PageInfo Msg
    , post_id_to_download : Int
    , post_id_to_download_err_status : Int
    , alert_modal_open : Modal.Visibility
    , alert_modal_text : String
    , reddit_listing_wrapper : Reddit.ListingWrapper
    , reddit_listing : Reddit.Listing
    , reddit_subreddit_to_download : String
    , reddit_is_downloaded : Bool
    , reddit_listing_page_info : Table.PageInfo Msg
    , current_tab : TabType
    , current_navbar_state : Navbar.State
    , current_weather_response : Weather.CurrentWeatherResponse
    , current_areas_str : String
    , form_definition : FormData.FormDefinition WeaponFrame Msg
    , form_data : WeaponFrame
    , dota_model : DotaModel
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


downloader : (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
downloader the_msg decoder =
    Http.get
        { url = root_json_server_url ++ "posts"
        , expect = Http.expectJson the_msg decoder
        }


download_all_posts : Cmd Msg
download_all_posts =
    downloader DownloadedAllPosts (list PostData.decode_single)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        parsedRoute =
            parseUrl url

        page_info =
            PageInfo navKey url (parseUrl url) UnsetPage

        post_data =
            PostData -1 "No Name" "No Title"

        reddit_listing =
            Reddit.Listing "" "" []

        reddit_listing_wrapper =
            Reddit.ListingWrapper "" reddit_listing

        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        current_weather_main =
            Weather.CurrentWeatherMain 0 0 0 0 0 0

        current_weather_response =
            Weather.CurrentWeatherResponse "" current_weather_main

        post_datas_page_info =
            Table.PageInfo 0 0 10 (PrevPageMsg PostDatasTable) (NextPageMsg PostDatasTable) (ChangePageMsg PostDatasTable)

        reddit_listing_page_info =
            Table.PageInfo 0 0 10 (PrevPageMsg RedditListingTable) (NextPageMsg RedditListingTable) (ChangePageMsg RedditListingTable)

        form_data : WeaponFrame
        form_data =
            { weapon_name = "unset in init wapn_ame", frame_id = 123, choice_id = -1 }

        form_definition : FormData.FormDefinition WeaponFrame Msg
        form_definition =
            let
                name_field : FormData.FormField WeaponFrame Msg
                name_field =
                    new_form_field_string "weapon_name" .weapon_name (Name >> UpdateFormData)

                frame_id_field : FormData.FormField WeaponFrame Msg
                frame_id_field =
                    new_form_field_int "frame_id" .frame_id (FrameId >> UpdateFormData)

                choice_id_field : FormData.FormField WeaponFrame Msg
                choice_id_field =
                    new_form_field_int "choice_id" .choice_id (ChoiceId >> UpdateFormData)
            in
            { fields =
                [ name_field
                , frame_id_field
                , choice_id_field
                ]
            }

        initial_tab =
            OpenDotaTab

        -- We use a Maybe so we don't have to worry about unset data
        -- dota_player_profile : OpenDota.PlayerProfile
        -- dota_player_profile =
        --     { account_id = -1
        --     , personaname = "Unset personaname"
        --     , name = "Unset name"
        --     , avatar = "Unset avatar"
        --     , avatarfull = "Unset avatarfull"
        --     }
        --
        -- dota_player_data : OpenDota.PlayerData
        -- dota_player_data =
        --     { profile = dota_player_profile }
        dota_model : DotaModel
        dota_model =
            { player_data = Nothing, account_id = 24801519, hero_stats = Nothing }

        initial_model : Model
        initial_model =
            { count = 0
            , content = "ASD"
            , time = Time.millisToPosix 0
            , zone = Time.utc
            , page_info = page_info
            , post_data = post_data
            , post_datas = []
            , post_datas_page_info = post_datas_page_info
            , post_id_to_download = -1
            , post_id_to_download_err_status = 0
            , alert_modal_open = Modal.hidden
            , alert_modal_text = ""
            , reddit_listing = reddit_listing
            , reddit_listing_wrapper = reddit_listing_wrapper
            , reddit_listing_page_info = reddit_listing_page_info
            , reddit_subreddit_to_download = ""
            , reddit_is_downloaded = False
            , current_tab = initial_tab
            , current_navbar_state = navbarState
            , current_weather_response = current_weather_response
            , current_areas_str = "Gatineau"
            , form_data = form_data
            , form_definition = form_definition
            , dota_model = dota_model
            }

        existingCmds =
            Cmd.batch
                [ --Task.perform AdjustTimeZone Time.here,
                  navbarCmd

                -- , Task.perform OnPageLoad Time.now
                , Task.perform (\_ -> DotaDownloadPlayerData 24801519) Time.now
                ]
    in
    initCurrentPage ( initial_model, existingCmds )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageLoad time ->
            let
                lg =
                    Debug.log "DEBUG: Page Loaded!" ""
            in
            ( model, Cmd.none )

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

        -- AdjustTimeZone newZone ->
        --     ( { model | zone = newZone }, Cmd.none )
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

                        page_info =
                            Table.initialize_page_info model.post_datas_page_info post_datas
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
            case model.reddit_subreddit_to_download of
                "" ->
                    ( model, Reddit.download_reddit_posts DownloadedRedditPosts )

                _ ->
                    -- ( model, Reddit.download_subreddit_posts model.reddit_subreddit_to_download DownloadedRedditPosts )
                    update (RequestJSONPFromSubreddit model.reddit_subreddit_to_download) model

        DownloadedRedditPostsJSONP listing_json_value ->
            --TODO: update page info as well
            let
                decoded_value =
                    case Json.Decode.decodeValue Reddit.decode_listing_wrapper listing_json_value of
                        Ok listing ->
                            listing

                        Err _ ->
                            model.reddit_listing_wrapper
            in
            ( { model | reddit_listing_wrapper = decoded_value }, Cmd.none )

        -- ( { model | reddit_listing_wrapper = listing }, Cmd.none )
        -- ( model, Reddit.download_reddit_posts )
        DownloadedRedditPosts result ->
            case result of
                Ok new_listing ->
                    let
                        lg =
                            Debug.log "Successfully received listing!"

                        listing =
                            new_listing

                        reddit_page_info =
                            Table.initialize_page_info model.reddit_listing_page_info listing.data.children
                    in
                    ( { model | reddit_listing_wrapper = listing, reddit_listing_page_info = reddit_page_info }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody err_msg ->
                            let
                                lg =
                                    Debug.log "PARSE ERROR: Error message handling the reddit post" "."

                                trimmed =
                                    String.slice 0 250 err_msg ++ "... trimmed"

                                lg2 =
                                    Debug.log trimmed "."
                            in
                            ( model, Cmd.none )

                        _ ->
                            ( Debug.log "Unknown error downloading" model, Cmd.none )

        ChangeTab new_tab ->
            ( { model | current_tab = new_tab }, Cmd.none )

        NavbarMsg state ->
            ( { model | current_navbar_state = state }, Cmd.none )

        DownloadCurrentWeather ->
            ( model, Weather.download_current_weather DownloadedCurrentWeather )

        ChangeCurrentWeatherArea new_areas_str ->
            ( { model | current_areas_str = new_areas_str }, Cmd.none )

        DownloadCurrentAreasWeather ->
            ( model, Weather.download_current_areas_weather model.current_areas_str DownloadedCurrentWeather )

        DownloadedCurrentWeather result ->
            case result of
                Ok new_weather ->
                    let
                        lg =
                            Debug.log "Current Weather Downloaded Succesfully" new_weather
                    in
                    ( { model | current_weather_response = new_weather }, Cmd.none )

                Err err_msg ->
                    let
                        lg =
                            Debug.log "Current Weather Download Error:" err_msg
                    in
                    ( model, Cmd.none )

        PrevPageMsg PostDatasTable ->
            let
                page_info =
                    model.post_datas_page_info

                new_page_info =
                    Table.decrement_page_idx page_info
            in
            ( { model | post_datas_page_info = new_page_info }, Cmd.none )

        PrevPageMsg RedditListingTable ->
            let
                page_info =
                    model.reddit_listing_page_info

                new_page_info =
                    Table.decrement_page_idx page_info
            in
            ( { model | reddit_listing_page_info = new_page_info }, Cmd.none )

        NextPageMsg PostDatasTable ->
            let
                page_info =
                    model.post_datas_page_info

                new_page_idx =
                    Table.increment_page_idx page_info
            in
            ( { model | post_datas_page_info = page_info }, Cmd.none )

        NextPageMsg RedditListingTable ->
            let
                page_info =
                    model.reddit_listing_page_info

                new_page_info =
                    Table.increment_page_idx page_info
            in
            ( { model | reddit_listing_page_info = new_page_info }, Cmd.none )

        ChangePageMsg PostDatasTable new_page_idx ->
            let
                page_info =
                    model.post_datas_page_info
            in
            ( { model | post_datas_page_info = { page_info | current_page_idx = new_page_idx } }, Cmd.none )

        ChangePageMsg RedditListingTable new_page_idx ->
            let
                page_info =
                    model.reddit_listing_page_info
            in
            ( { model | reddit_listing_page_info = { page_info | current_page_idx = new_page_idx } }, Cmd.none )

        ChangeSubredditToDownload new_subreddit ->
            ( { model | reddit_subreddit_to_download = new_subreddit }, Cmd.none )

        SendToPort str ->
            ( model, test_port_sending "This is from elm" )

        RequestJSONP str ->
            ( model, exec_jsonp <| "http://reddit.com/" ++ "r/" ++ "Games" ++ "/.json?jsonp=jsonpCallback" )

        RequestJSONPFromSubreddit subreddit ->
            ( model, exec_jsonp <| Reddit.subreddit_root_url subreddit ++ "/.json?jsonp=jsonpCallback" )

        RecvFromPort str ->
            ( Debug.log ("Received from port: " ++ str) model, Cmd.none )

        UpdateFormData form_update_type ->
            ( { model | form_data = update_form_data model.form_data form_update_type }, Cmd.none )

        SubmitFormData ->
            ( model, Cmd.none )

        DotaDownloadPlayerData account_id ->
            ( model, OpenDota.download_player_data account_id DotaDownloadedPlayerData )

        DotaDownloadHeroStats ->
            ( model, OpenDota.download_hero_stats DotaDownloadedHeroStats )

        -- DotaDownloadedPlayerData (Result Http.Error OpenDota.PlayerData)
        DotaDownloadedPlayerData response ->
            let
                _ =
                    Debug.log "Received a response: " response

                new_player_data =
                    case response of
                        Ok player_data ->
                            Just player_data

                        Err err ->
                            let
                                _ =
                                    Debug.log "Error: \n" err
                            in
                            Nothing

                dota_model =
                    model.dota_model

                new_dota_data =
                    { dota_model | player_data = new_player_data }
            in
            ( { model | dota_model = new_dota_data }, Cmd.none )

        DotaDownloadedHeroStats response ->
            let
                _ =
                    Debug.log "Received a response: " response

                new_hero_stats =
                    case response of
                        Ok hero_stats ->
                            Just hero_stats

                        Err err ->
                            let
                                _ =
                                    Debug.log "Error: \n" err
                            in
                            Nothing

                dota_model =
                    model.dota_model

                new_dota_data =
                    { dota_model | hero_stats = new_hero_stats }
            in
            ( { model | dota_model = new_dota_data }, Cmd.none )

        DotaUpdate dota_msg ->
            let
                ( dota_model, dota_cmd ) =
                    dota_update dota_msg model.dota_model
            in
            ( { model | dota_model = dota_model }, dota_cmd )


dota_update : DotaMsg -> DotaModel -> ( DotaModel, Cmd Msg )
dota_update msg dota_model =
    case msg of
        ChangeAccountId account_id ->
            ( { dota_model | account_id = account_id }, Cmd.none )


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


site_navigation : Model -> Html Msg
site_navigation model =
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
      , styles = []
      }
    , { column_id = "author"
      , idx = 1
      , pretty_title = "Author"
      , styles = []
      }
    , { column_id = "post_id"
      , idx = 0
      , pretty_title = "ID"
      , styles = []
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
    { title = Just "Post Datas", columns = my_column_defs }



--call a list of functions on an row


do_lookups : List (obj -> String) -> obj -> List String
do_lookups lookups row =
    List.foldl (\func acc -> acc ++ [ func row ]) [] lookups


nbsp : String
nbsp =
    "\u{00A0}"


temperature_val : Float -> Html Msg
temperature_val flt =
    span [] [ text <| String.fromFloat flt ++ "°" ]


form_data_view : Model -> Html Msg
form_data_view model =
    let
        form_data =
            model.form_data

        form_definition =
            model.form_definition
    in
    Grid.row []
        [ Grid.col [ Col.md6 ]
            [ Form.form []
                [ FormData.render_fields form_definition.fields form_data
                , button_primary DownloadRedditPosts "Submit"
                ]
            ]
        ]


weather_view : Model -> Html Msg
weather_view model =
    let
        is_weather_downloaded =
            case model.current_weather_response.name of
                "" ->
                    False

                _ ->
                    True

        weather_downloaded_str =
            case is_weather_downloaded of
                False ->
                    "No"

                True ->
                    "Yes"

        weather_main =
            model.current_weather_response.main

        weather_content =
            case is_weather_downloaded of
                False ->
                    div [] []

                True ->
                    div []
                        [ b [] [ text model.current_weather_response.name ]
                        , text " is currently: "
                        , temperature_val weather_main.temp
                        , text " But it feels more like: "
                        , temperature_val weather_main.feels_like
                        , br [] []
                        , text " You can expect temperatures as high as: "
                        , temperature_val weather_main.temp_max
                        , text " and as low as: "
                        , temperature_val weather_main.temp_min
                        ]
    in
    div []
        [ span [] [ text <| "Weather is downloaded? " ++ weather_downloaded_str ]
        , weather_content
        ]


listing_view : Model -> Html Msg
listing_view model =
    let
        ellipses_style =
            [ ( "max-width", "300px" )
            , ( "text-overflow", "ellipsis" )
            , ( "white-space", "nowrap" )
            , ( "overflow", "hidden" )
            ]

        column_defs =
            [ { column_id = "title", idx = 0, pretty_title = "Title", styles = ellipses_style }
            , { column_id = "url", idx = 1, pretty_title = "URL", styles = ellipses_style }
            , { column_id = "author", idx = 2, pretty_title = "Author", styles = [] }
            ]

        table_def =
            { title = Just "Submissions", columns = column_defs }

        column_lookups =
            [ \w -> w.data.title, \w -> w.data.url, \w -> w.data.author ]

        -- lookups =
        --     List.map .lookup_func column_lookups
        table_rows =
            List.map (do_lookups column_lookups) model.reddit_listing_wrapper.data.children
    in
    div []
        [ br [] []
        , Table.view table_def table_rows model.reddit_listing_page_info
        ]


navbar : Model -> Html Msg
navbar model =
    let
        nav_items : List ( TabType, String )
        nav_items =
            [ ( HomeTab, "Home" )
            , ( PostDataTableTab, "PostData Table" )
            , ( RedditListingTab, "Reddit Submissions Table" )
            , ( SinglePostDataTab, "Single PostData" )
            , ( WeatherTab, "Weather" )
            , ( FormDataTab, "Form Example" )
            , ( ModalTab, "Modal Example" )
            , ( OpenDotaTab, "OpenDota" )
            ]
    in
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "#" ] [ text "Home Page" ]
        |> Navbar.customItems
            (List.map
                (\( tab_type, txt ) ->
                    Navbar.textItem
                        [ onClick (ChangeTab tab_type)
                        , let
                            ( rule, val ) =
                                if model.current_tab == tab_type then
                                    ( "color", "blue" )

                                else
                                    ( "", "" )
                          in
                          style rule val
                        , style "margin-left" "10px"
                        , style "cursor" "pointer"
                        ]
                        [ text txt ]
                )
                nav_items
            )
        -- [ Navbar.itemLink [ onClick (ChangeTab RedditListingTab) ] [ text "Home" ]
        -- , Navbar.itemLink [ onClick (ChangeTab RedditListingTab) ] [ text "item 2" ]
        -- , Navbar.itemLink [ onClick (ChangeTab RedditListingTab) ] [ text "item 2" ]
        -- ]
        |> Navbar.view model.current_navbar_state


open_dota_view : DotaModel -> Html Msg
open_dota_view dota_model =
    let
        rendered_profile =
            case dota_model.player_data of
                Just player_data ->
                    let
                        player_profile =
                            player_data.profile
                    in
                    div []
                        [ div []
                            [ text <| "Player name: " ++ player_profile.personaname
                            ]
                        , div [] [ text <| "Est. MMR: " ++ String.fromInt player_data.mmr_estimate.estimate ]
                        , img [ src player_profile.avatarfull ] []
                        ]

                Nothing ->
                    div [] [ text "No downloaded player profile" ]

        rendered_hero_stats =
            case dota_model.hero_stats of
                Just hero_stats ->
                    div [] [ text "Downloaded Hero Stats" ]

                Nothing ->
                    div [] [ text "No hero stats downloaded" ]
    in
    div []
        [ h4 [] [ text "Open Dota!" ]
        , form []
            [ div [] [ button_primary DotaDownloadHeroStats "Download Hero Stats" ]
            , rendered_hero_stats
            , br [] []
            , button_primary (DotaDownloadPlayerData dota_model.account_id) "Download Profile"
            , input
                [ value <| String.fromInt dota_model.account_id
                , onInput
                    (\val ->
                        case String.toInt val of
                            Just valid_id ->
                                DotaUpdate <| ChangeAccountId valid_id

                            Nothing ->
                                DotaUpdate <| ChangeAccountId dota_model.account_id
                    )
                ]
                []
            ]
        , rendered_profile
        ]


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

        tab_content =
            case model.current_tab of
                HomeTab ->
                    div [] []

                SinglePostDataTab ->
                    div []
                        [ div []
                            [ text <| "Post data -- Title: " ++ model.post_data.title ++ ", and Author: " ++ model.post_data.author
                            ]
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
                            ]
                        ]

                PostDataTableTab ->
                    div []
                        [ button_primary DownloadAllPosts "Download All Posts"
                        , Table.view my_table_definition table_rows model.post_datas_page_info
                        ]

                RedditListingTab ->
                    div []
                        [ form [ onSubmit DownloadRedditPosts ]
                            [ button_primary DownloadRedditPosts "Download Reddit Data"
                            , input
                                [ value model.reddit_subreddit_to_download
                                , onInput ChangeSubredditToDownload
                                ]
                                []
                            ]
                        , listing_view model
                        ]

                WeatherTab ->
                    div []
                        [ h4 [] [ text "Weather!" ]
                        , form []
                            [ button_primary DownloadCurrentAreasWeather "Download Current Areas Weather"
                            , input
                                [ value model.current_areas_str
                                , onInput ChangeCurrentWeatherArea
                                ]
                                []
                            ]
                        , weather_view model
                        ]

                FormDataTab ->
                    div []
                        [ h4 [ add_class "testId" ] [ text "FormData!" ]
                        , form_data_view model
                        ]

                ModalTab ->
                    div []
                        [ h4 [] [ text "Modal Example" ]
                        , Button.button
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

                OpenDotaTab ->
                    div []
                        [ open_dota_view model.dota_model
                        ]
    in
    div [ add_class "container" ]
        [ div [ add_class "row" ]
            [ div [ add_class "col-md-12" ]
                [ site_navigation model
                , CDN.stylesheet
                ]
            , br [] []
            ]
        , button_primary (RequestJSONP "ASDS") "Port Send"
        , div [ add_class "row" ]
            [ div [ add_class "col-md-12" ]
                [ navbar model ]
            ]
        , br [] []
        , div [ add_class "row" ]
            [ div [ add_class "col-md-12" ] [ tab_content ]
            ]
        , br [] []
        ]



-- turns ['a', 'b', 'c'] -> [(0, 'a'), (1, 'b'), (2, 'c')]


join_with_numbers : List a -> List ( Int, a )
join_with_numbers to_join =
    List.map2 Tuple.pair (List.range 0 (List.length to_join)) to_join


profileView : Model -> Html Msg
profileView model =
    div []
        [ site_navigation model
        , text "Welcome to my Profile!!"
        ]


port test_port_receiving : (String -> msg) -> Sub msg


port test_port_sending : String -> Cmd msg



-- port recv_reddit_listing : (Reddit.ListingWrapper -> msg) -> Sub msg


port recv_reddit_listing : (Json.Decode.Value -> msg) -> Sub msg


port exec_jsonp : String -> Cmd msg



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
