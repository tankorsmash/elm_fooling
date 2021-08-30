module VisualOutput exposing (Model, Msg(..), init, update, view)

import Color
import Color.Convert as Convert
import Element exposing (Color, Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, fillPortion, height, padding, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html


type alias Model =
    { entries : List String, show_modal : Bool }


type Msg
    = ShowModal (Maybe String)


init : Model
init =
    { entries = []
    , show_modal = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowModal maybe_new_entry ->
            case maybe_new_entry of
                Just new_entry ->
                    ( { model | show_modal = not model.show_modal, entries = new_entry :: model.entries }, Cmd.none )

                Nothing ->
                    ( { model | show_modal = not model.show_modal }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        in_front =
            if model.show_modal then
                Element.inFront <| modal <| Just <| String.join "\n" model.entries

            else
                -- TODO come up with a better way to return nothing from this
                Element.inFront <| el [] <| text ""
    in
    Element.layout [ in_front ] <| column [] []


modal : Maybe String -> Element.Element Msg
modal maybe_entries =
    let
        entries =
            case maybe_entries of
                Just "" ->
                    "No VisualOutput entries found in model"

                Just entries_ ->
                    entries_

                Nothing ->
                    "IN FRONT"
    in
    row
        [ width fill
        , height fill
        , Font.center
        , Background.color <| rgb 0.5 0.5 1
        , Events.onClick <| ShowModal Nothing

        -- , Element.explain Debug.todo
        ]
        [ column [ centerX ] [ text entries ] ]
