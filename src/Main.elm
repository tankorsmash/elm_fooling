port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Debug
import Element
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
import Html.Lazy
import Http
import ItemShop
import Json.Decode as Decode exposing (Decoder, at, field, list, string)
import Json.Encode as Encode exposing (Value, string)
import List
import Sfxr
import String
import Task
import Time
import Url
import Url.Parser exposing ((</>), Parser, fragment, int, map, oneOf, parse, s, string)
import Utils exposing (add_class)


type Msg
    = OnPageLoad Time.Posix
    | Tick Time.Posix
      -- | AdjustTimeZone Time.Zone
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeTab TabType
    | SendToPort String
    | RequestJSONP String
    | RecvFromPort String
      -- | GotEditWeaponFormUpdate Magnolia.WeaponFrame.EditFormUpdateType
      -- | SubmitFormData
    | GotItemShopMsg ItemShop.Msg
    | GotSfxrMsg Sfxr.Msg
    | OnWindowResize Int Int



-- MAIN
-- form_data


root_json_server_url =
    "http://localhost:5021/"


root_data_json_server_url =
    "http://localhost:4126/"


add_class cls =
    property "className" (Encode.string cls)


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
          test_port_receiving RecvFromPort
        , Sub.map GotItemShopMsg <| ItemShop.subscriptions model.item_shop_model
        , Sub.map GotSfxrMsg <| Sfxr.subscriptions model.sfxrModel
        , Browser.Events.onResize OnWindowResize
        ]



-- MODEL


type alias UrlPageInfo =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , page : Page
    }


type TabType
    = ItemShopTab


type alias Model =
    { count : Int
    , content : String
    , time : Time.Posix
    , zone : Time.Zone
    , page_info : UrlPageInfo
    , current_tab : TabType
    , item_shop_model : ItemShop.Model
    , sfxrModel : Sfxr.Model
    , device : Element.Device
    }


type Page
    = NotFoundPage
    | HomePage
    | UnsetPage


type Route
    = TabRoute TabType (Maybe String)
    | NotFound
    | UnsetRoute


parseUrl : Url.Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            case route of
                TabRoute tab_type hash ->
                    TabRoute tab_type hash

                _ ->
                    TabRoute ItemShopTab Nothing

        Nothing ->
            TabRoute ItemShopTab Nothing


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map (TabRoute ItemShopTab) (s "item_shop_tab" </> fragment identity)
        , map (TabRoute ItemShopTab) (s "" </> fragment identity)
        , map (TabRoute ItemShopTab) (fragment identity)
        ]


init : { window : { width : Int, height : Int } } -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        parsedRoute : Route
        parsedRoute =
            parseUrl url

        initial_tab : TabType
        initial_tab =
            case parsedRoute of
                TabRoute tab_type hsh ->
                    tab_type

                _ ->
                    ItemShopTab

        device =
            Element.classifyDevice flags.window

        page_info =
            UrlPageInfo navKey url parsedRoute UnsetPage

        hash : String
        hash =
            case parsedRoute of
                TabRoute tab_type (Just hash_) ->
                    hash_

                _ ->
                    ""

        ( item_shop_model, item_shop_cmds ) =
            ItemShop.init device hash (Just navKey)

        initial_model : Model
        initial_model =
            { count = 0
            , content = "ASD"
            , time = Time.millisToPosix 0
            , zone = Time.utc
            , page_info = page_info
            , current_tab = initial_tab
            , item_shop_model = item_shop_model
            , sfxrModel = Sfxr.init
            , device = device
            }

        existingCmds : Cmd Msg
        existingCmds =
            Cmd.batch
                [ Cmd.map GotItemShopMsg item_shop_cmds
                ]
    in
    initCurrentPage ( initial_model, existingCmds )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.page_info.route of
                TabRoute tab_type fragment ->
                    ( HomePage, Cmd.none )

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

                newRoute : Route
                newRoute =
                    Debug.log "parsed Route" <| parseUrl url

                fragment : String
                fragment =
                    case newRoute of
                        -- Home (Just hash) ->
                        TabRoute tab_type (Just hash) ->
                            -- Debug.log "url changed" hash
                            case tab_type of
                                ItemShopTab ->
                                    let
                                        _ =
                                            Debug.log "Main.tab_type changed to " tab_type
                                    in
                                    hash

                        _ ->
                            Debug.log "empty hash on UrlChanged" ""

                new_model =
                    { model
                        | page_info = { page_info | url = url, route = newRoute }
                    }
            in
            ( new_model
            , Cmd.none
            )
                |> initCurrentPage

        ChangeTab new_tab ->
            let
                oldItemShopModel =
                    model.item_shop_model

                --force pausing the item shop, because its annoying to have model updates if the item shop isnt being worked on
                newItemShopModel =
                    { oldItemShopModel | ai_updates_paused = True }
            in
            ( { model | current_tab = new_tab, item_shop_model = newItemShopModel }, Cmd.none )

        SendToPort str ->
            ( model, test_port_sending "This is from elm" )

        RequestJSONP str ->
            ( model, exec_jsonp <| "http://reddit.com/" ++ "r/" ++ "Games" ++ "/.json?jsonp=jsonpCallback" )

        RecvFromPort str ->
            ( Debug.log ("Received from port: " ++ str) model, Cmd.none )

        GotItemShopMsg item_shop_msg ->
            let
                ( sub_model, sub_cmd ) =
                    ItemShop.update item_shop_msg model.item_shop_model
            in
            ( { model
                | item_shop_model = sub_model
              }
            , Cmd.map GotItemShopMsg sub_cmd
            )

        GotSfxrMsg sfxrMsg ->
            let
                ( sub_model, sub_cmd ) =
                    Sfxr.update sfxrMsg model.sfxrModel
            in
            ( { model
                | sfxrModel = sub_model
              }
            , Cmd.map GotSfxrMsg sub_cmd
            )

        OnWindowResize width height ->
            let
                newDevice =
                    Element.classifyDevice
                        { width = width, height = height }
            in
            ( { model | device = newDevice }, Cmd.none )



-- end of update


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
--call a list of functions on an row


nbsp : String
nbsp =
    "\u{00A0}"


temperature_val : Float -> Html Msg
temperature_val flt =
    span [] [ text <| String.fromFloat flt ++ "°" ]



-- form_data_view : Model -> Html Msg
-- form_data_view model =


tab_type_to_str : TabType -> String
tab_type_to_str tab_type =
    case tab_type of
        ItemShopTab ->
            "Item Shop"


title_case : String -> String
title_case raw_str =
    case String.uncons raw_str of
        Just ( letter, rest ) ->
            let
                maybe_first_letter =
                    List.head <| String.toList <| String.toUpper <| String.fromChar <| letter
            in
            case maybe_first_letter of
                Just first_letter ->
                    String.cons first_letter rest

                Nothing ->
                    raw_str

        Nothing ->
            raw_str


homeView : Model -> Html Msg
homeView model =
    let
        tab_content =
            case model.current_tab of
                ItemShopTab ->
                    Html.map GotItemShopMsg <|
                        Html.Lazy.lazy ItemShop.view model.item_shop_model

        --elm ui needs exactly one layout to exist with the stylesheet, all others
        -- need the `noStaticStyleSheet` option set, and force its height to 0
        elm_ui_hack_layout =
            div [ Html.Attributes.style "height" "0" ]
                [ Element.layoutWith
                    { options =
                        [ Element.focusStyle
                            { borderColor = Nothing
                            , backgroundColor = Nothing
                            , shadow = Nothing
                            }
                        ]
                    }
                    [ Element.htmlAttribute <| Html.Attributes.id "hack" ]
                  <|
                    Element.none
                ]
    in
    div []
        [ elm_ui_hack_layout
        , tab_content
        ]


{-| turns ['a', 'b', 'c'] -> [(0, 'a'), (1, 'b'), (2, 'c')]
-}
join_with_numbers : List a -> List ( Int, a )
join_with_numbers to_join =
    List.map2 Tuple.pair (List.range 0 (List.length to_join)) to_join


port test_port_receiving : (String -> msg) -> Sub msg


port test_port_sending : String -> Cmd msg



-- port recv_reddit_listing : (Reddit.ListingWrapper -> msg) -> Sub msg


port recv_reddit_listing : (Decode.Value -> msg) -> Sub msg


port exec_jsonp : String -> Cmd msg


viewSfxr model =
    if True then
        div []
            [ Html.map GotSfxrMsg <| Sfxr.view model.sfxrModel ]

    else
        div [] []



-- view : Model -> Html Msg


view : Model -> Browser.Document Msg
view model =
    { title = tab_type_to_str model.current_tab ++ " | Elm Fooling"
    , body =
        [ --viewSfxr model
          case model.page_info.page of
            NotFoundPage ->
                div [] [ text "Not found page" ]

            HomePage ->
                homeView model

            UnsetPage ->
                div [] [ text "UNSET PAGE" ]
        ]
    }
