module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Debug --for prints

import Task
import Time


-- MAIN


main =
    Browser.element {
        init = init, view = view, update = update2,
        subscriptions = subscriptions
    }



subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

-- MODEL


type alias Model = {
    count : Int,
    content : String,
    time : Time.Posix,
    zone : Time.Zone
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( Model 0 "ASD" (Time.millisToPosix 0) Time.utc
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE



type Msg
    = Increment
    | Decrement
    | Poop
    | Change String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


update2 : Msg -> Model -> (Model, Cmd Msg)
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


my_func : Int -> Int
my_func age = age * 100


custom_input : String -> String -> String -> (String -> msg) -> Html msg
custom_input type__ placeholder_ value_ toMsg =
    input [ type_ type__, placeholder placeholder_, value value_, onInput toMsg ] []


display_validation : String -> Html msg
display_validation to_validate =
    if to_validate == "valid" then
        div [style "color" "green"] [text "MFer is valid" ]
    else if True then
        div [] [text "true"]
    else
        div [] [text ("try again" ++ (Debug.log ("to_validate: "++to_validate) ""))]


humanize : Time.Posix -> Time.Zone -> String
humanize time zone =
    let
        hour = String.fromInt (Time.toHour zone time)
        minute = String.fromInt (Time.toMinute zone time)
        second = String.fromInt (Time.toSecond zone time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second

-- VIEW


view : Model -> Html Msg
view model =
    div []
    [
        -- custom_input "text" "ppplllace" model.content Change
        custom_input "text" "ppplllace" (humanize model.time model.zone) Change
        , display_validation model.content
    -- button [ onClick Decrement ] [ text "-" ]
    -- , div [] [ text (String.fromInt model.count) ]
    -- , button [ onClick Increment ] [ text "+" ]
    -- , div [] [ text (model.content ++ ", the length is: "++ String.fromInt (String.length model.content))]
    -- , div [] [ text (String.fromInt (my_func model.count))]
    -- , input [ placeholder "placeholder", value model.content, onInput Change] []
    ]

