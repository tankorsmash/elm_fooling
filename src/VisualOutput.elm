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
import Html.Attributes


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
                Element.inFront <| modal <| Just <| model.entries

            else
                -- TODO come up with a better way to return nothing from this
                Element.inFront <| Element.none
    in
    -- Element.layout
    Element.layoutWith
        { options =
            [ Element.noStaticStyleSheet
            , Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ in_front
        , Element.htmlAttribute <| Html.Attributes.id "visualoutput"

        -- , Element.explain Debug.todo
        , Element.height (Element.fill |> Element.maximum 1)
        ]
    <|
        Element.none


modal : Maybe (List String) -> Element.Element Msg
modal maybe_entries =
    let
        entries =
            case maybe_entries of
                Just [] ->
                    [ "No VisualOutput entries found in model" ]

                Just entries_ ->
                    entries_

                Nothing ->
                    [ "IN FRONT" ]

        build_paras =
            \entry -> text entry

        replace_escaped_quotes : String -> String
        replace_escaped_quotes str =
            String.replace "\\\"" "\"" str

        split_on_newlines : String -> List String
        split_on_newlines str =
            String.split "\\n" str
    in
    row
        [ width fill
        , height fill
        , Font.center
        , Background.color <| rgb 0.5 0.5 1
        , Events.onClick <| ShowModal Nothing

        -- , Element.explain Debug.todo
        ]
        [ column
            [ centerX
            , width fill
            , Font.family [ Font.monospace ]
            ]
          <|
            List.map build_paras <|
                List.concat <|
                    List.map
                        (split_on_newlines << replace_escaped_quotes)
                        entries
        ]
