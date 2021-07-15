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


-- MAIN


main =
    Browser.sandbox { init = init, update = update2, view = view }



-- MODEL


type alias Model = {
    count : Int,
    content : String
    }


init : Model
init =
    Model 0 "ASD"



-- UPDATE



type Msg
    = Increment
    | Decrement
    | Poop
    | Change String


update2 : Msg -> Model -> Model
update2 msg model =
    case msg of
        Increment ->
            { model | count = model.count + 10}

        Decrement ->
            { model | count = model.count - 1}

        Poop ->
            { model | count = model.count + 2}

        Change newContent ->
            { model | content = newContent }

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

-- VIEW


view : Model -> Html Msg
view model =
    div []
    [
        custom_input "text" "ppplllace" model.content Change
        , display_validation model.content
    -- button [ onClick Decrement ] [ text "-" ]
    -- , div [] [ text (String.fromInt model.count) ]
    -- , button [ onClick Increment ] [ text "+" ]
    -- , div [] [ text (model.content ++ ", the length is: "++ String.fromInt (String.length model.content))]
    -- , div [] [ text (String.fromInt (my_func model.count))]
    -- , input [ placeholder "placeholder", value model.content, onInput Change] []
    ]

