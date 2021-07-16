module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, span, div, text, input, b, a)
-- import Html exposing (..)
import Html.Attributes exposing (href, style)
-- import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, parse)

import Http
import Json.Decode exposing (Decoder, field, string, list)


import List

import Debug --for prints

import Task
import Time


-- MAIN


main =
    -- Browser.element {
    --     init = init, view = view, update = update2,
    --     subscriptions = subscriptions
    -- }
    Browser.application {
        init = init, view = view, update = update2,
        subscriptions = subscriptions,
        onUrlChange = UrlChanged,
        onUrlRequest = LinkClicked
    }



subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

-- MODEL

type alias PostData =
    { id : Int
    , title : String
    , author : String
    }

type alias PageInfo =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , page : Page
    }

type alias Model =
    { count : Int
    , content : String
    , time : Time.Posix
    , zone : Time.Zone
    , page_info : PageInfo
    , post_data : PostData
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
        [ map Home       (s "home")
        , map Profile    (s "profile")
        -- , map Topic   (s "topic" </> string)
        -- , map Blog    (s "blog" </> int)
        -- , map User    (s "user" </> string)
        -- , map Comment (s "user" </> string </> s "comment" </> int)
        ]


download_all_posts : Cmd Msg
download_all_posts =
    Http.get
        { url = "http://localhost:5021/posts"
        , expect = Http.expectJson GotJSON decode_post_titles
        }

decode_post_titles : Decoder (List String)
decode_post_titles =
    list (field "title" Json.Decode.string)

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url navKey =
    let
        parsedRoute = parseUrl url
        page_info = PageInfo navKey url (parseUrl url) UnsetPage
        post_data = PostData -1 "No Name" "No Title"
        model = Model 0 "ASD" (Time.millisToPosix 0) Time.utc page_info post_data

        existingCmds = Task.perform AdjustTimeZone Time.here
    in
        initCurrentPage (model, existingCmds)


initCurrentPage : (Model, Cmd Msg) -> (Model, Cmd Msg)
initCurrentPage (model, existingCmds) =
    let
        ( currentPage ,mappedPageCmds ) =
            case model.page_info.route of
                Home ->
                    ( HomePage, Cmd.none )

                Profile ->
                    ( ProfilePage, Cmd.none )

                NotFound ->
                    ( NotFoundPage, Cmd.none )

                UnsetRoute ->
                    ( NotFoundPage, Cmd.none )
        page_info = model.page_info
    in
    ({model | page_info = {page_info | page = currentPage}}
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )




-- UPDATE


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
    | GotJSON (Result Http.Error (List String))


update2 : Msg ->  Model -> (Model, Cmd Msg)
update2 msg model =
    case msg of
        Increment ->
            ({ model | count = model.count + 10}, Cmd.none)

        Decrement ->
            ({ model | count = model.count - 1}, Cmd.none)

        Poop ->
            ({ model | count = model.count + 2}, Cmd.none)

        Change newContent ->
            ({ model | content = newContent }, Cmd.none)

        Tick newTime ->
            ({ model | time = newTime }, Cmd.none)

        AdjustTimeZone newZone ->
            ({ model | zone = newZone }, Cmd.none)

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.page_info.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)

        UrlChanged url ->
            let
                page_info = model.page_info
                newRoute = parseUrl url
            in
                ( { model | page_info = { page_info | url = url, route=newRoute } }
                , Cmd.none
                ) |> initCurrentPage

        DownloadAllPosts ->
            (model, download_all_posts)

        GotJSON result ->
            case result of
                Ok titles ->
                    let
                        post_data = Debug.log "Successfully received files!" model.post_data
                        title = List.head titles
                    in
                        case title of
                            Just title_ ->
                                ({model | post_data = { post_data | title=title_ }}, Cmd.none)
                            Nothing ->
                                ({model | post_data = { post_data | title="Error" }}, Cmd.none)

                Err error ->
                    case error of
                        Http.BadBody err_msg ->
                            (Debug.log err_msg model, Cmd.none)
                        _ ->
                            (Debug.log ("Unknown error downloading") model, Cmd.none)



my_func : Int -> Int
my_func age = age * 100


-- custom_input : String -> String -> String -> (String -> msg) -> Html msg
-- custom_input type__ placeholder_ value_ toMsg =
--     input [ type_ type__, placeholder placeholder_, value value_, onInput toMsg ] []


-- display_validation : String -> Html msg
-- display_validation to_validate =
--     if to_validate == "valid" then
--         div [style "color" "green"] [text "MFer is valid" ]
--     else if True then
--         div [] [text "true"]
--     else
--         div [] [text ("try again" ++ (Debug.log ("to_validate: "++to_validate) ""))]
--

humanize : Time.Posix -> Time.Zone -> String
humanize time zone =
    let
        hour = String.fromInt (Time.toHour zone time)
        minute = String.fromInt (Time.toMinute zone time)
        second = String.fromInt (Time.toSecond zone time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second

-- VIEW

navigation : Model -> Html Msg
navigation model =
    div [ style "margin-bottom" "15px"]
    [ div []
        [ text "This is a link: "
        , a [href "/home" ] [ text "HOME" ]
        ]
    , div []
        [ text "This is my profile: "
        , a [href "/profile" ] [ text "PROFILE" ]
        ]
    ]

homeView : Model -> Html Msg
homeView model =
    div []
        [ navigation  model
        , text "HOME PAGE IS HERE!!!"
        , div [] [
            text <| "Post data -- Title: " ++ model.post_data.title ++ ", and Author: " ++ model.post_data.author
            ]
        , button [onClick DownloadAllPosts] [ text "Download JSON" ]
        ]

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
        [
        case model.page_info.page of
            NotFoundPage ->
                div [] [ text "Not found page"]

            HomePage ->
                homeView model

            ProfilePage ->
                profileView model

            UnsetPage ->
                div [] [ text "UNSET PAGE"]
        ]
    }

