module Magnolia.FrameView exposing (view, Msg)

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
import Magnolia.WeaponFrame exposing (WeaponFrame)
import OpenDota.OpenDota as OpenDota
import PostData exposing (PostData)
import Reddit
import String
import Table exposing (ColumnDef, ColumnType(..), PageInfoMsg, TableDefinition, update_page_info, view)
import Task
import Time
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)
import Utils exposing (add_class)
import Weather

type Msg
    = SubmitFormData

type alias Model =
    {weapon_edit_form_definition : FormData.FormDefinition WeaponFrame Msg
    -- { edit_form_definition: Magnolia.WeaponFrame.edit_form_definition GotEditWeaponFormUpdate}
    , form_data : WeaponFrame
    , saved_form_data : Maybe WeaponFrame
    }

update : Model -> Msg -> (Model, Cmd Msg)
update model msg =
    case msg of
        SubmitFormData ->
            ( { model | saved_form_data = Just model.form_data }, Cmd.none )


-- model.form_data model.weapon_edit_form_definition model.saved_form_data
bootstrap_button type_ on_click text_ =
    Button.button
        [ type_
        , Button.attrs [ onClick on_click ]
        ]
        [ text text_ ]


button_primary : msg -> String -> Html msg
button_primary on_click text_ =
    bootstrap_button Button.primary on_click text_

view : model -> Html Msg
view model = div [] [text "Frame View"]

form_data_view : obj -> FormData.FormDefinition obj Msg -> Maybe obj -> Html Msg
form_data_view form_data form_definition maybe_saved_form_data =
    let
        rendered_saved_form_data : Html Msg
        rendered_saved_form_data =
            case maybe_saved_form_data of
                Nothing ->
                    div [] []

                Just saved_form_data ->
                    div []
                        [ div [] [ text "Saved Form Data" ]
                        , FormData.render_fields form_definition.fields saved_form_data
                        , br [] []
                        ]
    in
    Grid.row []
        [ Grid.col [ Col.md6 ]
            [ rendered_saved_form_data
            , Form.form []
                [ FormData.render_fields form_definition.fields form_data
                , button_primary SubmitFormData "Submit"
                ]
            ]
        ]
