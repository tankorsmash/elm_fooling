module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, text, input, b, a)
-- import Html exposing (..)
import Html.Attributes exposing (href)
-- import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, parse)


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



subscriptions :  Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

-- MODEL


type alias Model =
    { count : Int
    , content : String
    , time : Time.Posix
    , zone : Time.Zone
    , key : Nav.Key
    , url : Url.Url
    , route : Route
    , page : Page
    }

type Page
    = NotFoundPage
    | HomePage
    | UnsetPage

type Route
    = Home
    | NotFound
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
        [ map Home    (s "home")
        -- , map Topic   (s "topic" </> string)
        -- , map Blog    (s "blog" </> int)
        -- , map User    (s "user" </> string)
        -- , map Comment (s "user" </> string </> s "comment" </> int)
        ]


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url navKey =
    let
        parsedRoute = parseUrl url
        model = Model 0
            "ASD" (Time.millisToPosix 0) Time.utc navKey url
            (parseUrl url)
            UnsetPage
        existingCmds = Task.perform AdjustTimeZone Time.here
    in
        initCurrentPage (model, existingCmds)


initCurrentPage : (Model, Cmd Msg) -> (Model, Cmd Msg)
initCurrentPage (model, existingCmds) =
    let
        ( currentPage ,mappedPageCmds ) =
            case model.route of
                Home ->
                    ( HomePage, Cmd.none )

                NotFound ->
                    ( NotFoundPage, Cmd.none )

                UnsetRoute ->
                    ( NotFoundPage, Cmd.none )
    in
    ( { model | page = currentPage }
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
            Debug.log "TICKING" ({ model | time = newTime }, Cmd.none)

        AdjustTimeZone newZone ->
            ({ model | zone = newZone }, Cmd.none)

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)

        UrlChanged url ->
            ( {model | url = url }
            , Cmd.none
            )


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

homeView : Model -> Html Msg
homeView model =
    div []
        [ text "HOME PAGE IS HERE!!!" ]


-- view : Model -> Html Msg
view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [
        case model.page of
            NotFoundPage ->
                div [] [ text "Not found page"]

            HomePage ->
                homeView model

            UnsetPage ->
                div [] [ text "UNSET PAGE"]
        ]
    }

