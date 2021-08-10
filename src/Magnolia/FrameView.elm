module Magnolia.FrameView exposing (Model, Msg, init, update, view)

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
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
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
    | GotEditWeaponFormUpdate Magnolia.WeaponFrame.EditFormUpdateType
    | TabMsg Tab.State


type alias Model =
    { weapon_edit_form_definition : FormData.FormDefinition WeaponFrame Msg
    , weapon_frame_data : WeaponFrame
    , saved_weapon_frame_data : Maybe WeaponFrame
    , active_tab : Tab.State
    }


init : Model
init =
    let
        weapon_frame_data : WeaponFrame
        weapon_frame_data =
            { weapon_name = "unset in init wapn_ame"
            , frame_id = 123
            , choice_id = -1
            , pretty_name = "Pretty Wepn Name"
            , description = "This is a description"
            , frame_image_path = "weapon_img.png"
            , battle_row_type = Magnolia.WeaponFrame.Rear
            , damage_type = Magnolia.WeaponFrame.Slashing
            , bonus_attack = 0
            , bonus_power = 0
            , bonus_encumbrance = 0
            , rarity_type = 0
            , carry_weight = 0
            }

        saved_weapon_frame_data : Maybe WeaponFrame
        saved_weapon_frame_data =
            Nothing
    in
    { weapon_edit_form_definition = Magnolia.WeaponFrame.edit_form_definition GotEditWeaponFormUpdate
    , weapon_frame_data = weapon_frame_data
    , saved_weapon_frame_data = saved_weapon_frame_data
    , active_tab = Tab.initialState
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        SubmitFormData ->
            ( { model | saved_weapon_frame_data = Just model.weapon_frame_data }, Cmd.none )

        GotEditWeaponFormUpdate form_update_type ->
            ( { model | weapon_frame_data = Magnolia.WeaponFrame.update_edit_form_data model.weapon_frame_data form_update_type }, Cmd.none )

        TabMsg new_state ->
            ( { model | active_tab = new_state }, Cmd.none )


bootstrap_button type_ on_click text_ =
    Button.button
        [ type_
        , Button.attrs [ onClick on_click ]
        ]
        [ text text_ ]


button_primary : msg -> String -> Html msg
button_primary on_click text_ =
    bootstrap_button Button.primary on_click text_


tabs_view : Model -> Html Msg
tabs_view model =
    Tab.config TabMsg
        |> Tab.items
            [ Tab.item
                { id = "tab_item_1"
                , link = Tab.link [] [ text "Edit WeaponFrame" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ]
                        [ h4 [] [ text "Edit WeaponFrame" ]
                        , form_data_view model.weapon_frame_data model.weapon_edit_form_definition model.saved_weapon_frame_data
                        ]
                }
            , Tab.item
                { id = "tab_item_2"
                , link = Tab.link [] [ text "Tab Item 2" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ]
                        [ h4 [] [ text "Tab 2 Heading" ]
                        , p [] [ text "TODO" ]
                        ]
                }
            ]
        |> Tab.view model.active_tab


view : Model -> Html Msg
view model =
    div []
        [ tabs_view model
        ]


form_data_view : obj -> FormData.FormDefinition obj Msg -> Maybe obj -> Html Msg
form_data_view weapon_frame_data form_definition maybe_saved_form_data =
    let
        rendered_saved_form_data : Html Msg
        rendered_saved_form_data =
            case maybe_saved_form_data of
                Nothing ->
                    div [] []

                Just saved_weapon_frame_data ->
                    div []
                        [ div [] [ text "Saved Form Data" ]
                        , FormData.render_fields form_definition.fields saved_weapon_frame_data
                        , br [] []
                        ]
    in
    Grid.row []
        [ Grid.col [ Col.md6 ]
            [ rendered_saved_form_data
            , Form.form []
                [ FormData.render_fields form_definition.fields weapon_frame_data
                , button_primary SubmitFormData "Submit"
                ]
            ]
        ]
