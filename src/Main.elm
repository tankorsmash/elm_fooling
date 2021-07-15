module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



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
    { count = 0, content= "ASD" }



-- UPDATE



type Msg
    = Increment
    | Decrement
    | Poop


update2 : Msg -> Model -> Model
update2 msg model =
    case msg of
        Increment ->
            { model | count = model.count + 10}

        Decrement ->
            { model | count = model.count - 1}

        Poop ->
            { model | count = model.count + 2}

my_func : Int -> Int
my_func age = age * 100

-- VIEW


view : Model -> Html Msg
view model =
    div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [] [ text "adsd"]
    , div [] [ text (String.fromInt (my_func model.count))]
    ]

