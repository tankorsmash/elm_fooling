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
import Magnolia.ArmorFrame exposing (ArmorFrame)
import Magnolia.AttributeFrame exposing (AttributeFrame)
import Magnolia.BattleTextStructFrame exposing (BattleTextStructFrame)
import Magnolia.WeaponCategoryFrame exposing (WeaponCategoryFrame)
import Magnolia.WeaponFrame exposing (WeaponFrame)
import Magnolia.ZoneFrame exposing (ZoneFrame)
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

type GotFrameEditFormUpdateMsg
    = GotEditWeaponFormUpdate Magnolia.WeaponFrame.EditFormUpdateType
    | GotEditZoneFormUpdate Magnolia.ZoneFrame.EditFormUpdateType
    | GotEditWeaponCategoryFormUpdate Magnolia.WeaponCategoryFrame.EditFormUpdateType
    | GotEditAttributeFormUpdate Magnolia.AttributeFrame.EditFormUpdateType
    | GotEditBattleTextStructFormUpdate Magnolia.BattleTextStructFrame.EditFormUpdateType
    | GotEditArmorFormUpdate Magnolia.ArmorFrame.EditFormUpdateType

type Msg
    = ToggleFrameViewMode
    | GotFrameEditFormUpdate GotFrameEditFormUpdateMsg
    | GotPageMsg FrameType PageInfoMsg
    | GotTabMsg Tab.State


type alias FrameEditData f msg =
    { form_definition : FormData.FormDefinition f msg
    , frame_data : f
    , saved_frame_data : Maybe f
    , table_view_page_info : Table.PageInfo msg
    }


type FrameType
    = WeaponFrame
    | ArmorFrame
    | ZoneFrame
    | WeaponCategoryFrame
    | AttributeFrame
    | BattleTextStructFrame


type alias FrameEditDatas =
    { weapon : FrameEditData WeaponFrame Msg
    , armor : FrameEditData ArmorFrame Msg
    , zone : FrameEditData ZoneFrame Msg
    , weapon_category : FrameEditData WeaponCategoryFrame Msg
    , attribute : FrameEditData AttributeFrame Msg
    , battle_text_struct : FrameEditData BattleTextStructFrame Msg
    }


type FrameViewMode
    = List
    | Edit


type alias Model =
    { frame_edit_datas : FrameEditDatas
    , frame_view_mode : FrameViewMode
    , active_tab : Tab.State
    }


tab_prefix : String
tab_prefix =
    "frame_view_tab__"


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

        armor_frame_data : ArmorFrame
        armor_frame_data =
            { pretty_name = "Unset ArmorFrame Name"
            , frame_id = 123
            , bonus_defense = 0
            , bonus_protection = 0
            , bonus_protection_piercing = 0
            , bonus_protection_blunt = 0
            , bonus_protection_slashing = 0
            , bonus_encumbrance = 0
            , rarity_type = 0
            , carry_weight = 0
            }

        saved_armor_frame_data : Maybe ArmorFrame
        saved_armor_frame_data =
            Nothing

        zone_frame_data : ZoneFrame
        zone_frame_data =
            { name = "unset in init zone"
            , data_name = "unset_data_name"
            , required_zone_data_name_to_unlock = ""
            , location_data_names_in_the_zone = [ "first", "second" ]
            }

        saved_zone_frame_data : Maybe ZoneFrame
        saved_zone_frame_data =
            Nothing

        weapon_category_frame_data : WeaponCategoryFrame
        weapon_category_frame_data =
            { frame_id = 0
            , pretty_name = "unset in init weapon_category"
            , description = "description"
            , frame_image_path = "unset .png"
            , rarity_type = 0
            , weapon_frame_ids = []
            , rank_1_attr_frame_ids = [ 1, 11, 111 ]
            , rank_2_attr_frame_ids = [ 2, 22, 222 ]
            , rank_3_attr_frame_ids = [ 3, 33, 333 ]
            }

        saved_weapon_category_frame_data : Maybe WeaponCategoryFrame
        saved_weapon_category_frame_data =
            Nothing

        attribute_frame_data : AttributeFrame
        attribute_frame_data =
            { frame_id = 0
            , pretty_name = "unset in init attribute"
            , description = "description"
            , frame_image_path = "unset .png"
            , state_names = []
            , state_names_pretty_funcs = []
            , pretty_name_template = ""
            , pretty_func_name = ""
            }

        saved_attribute_frame_data : Maybe AttributeFrame
        saved_attribute_frame_data =
            Nothing

        battle_text_struct_frame_data : BattleTextStructFrame
        battle_text_struct_frame_data =
            { frame_id = 0
            , pretty_name = "unset in init battle_text_struct"
            , description = "description"
            , frame_image_path = "unset .png"
            , state_names = []
            , state_names_pretty_funcs = []
            , pretty_name_template = ""
            , pretty_func_name = ""
            }

        saved_battle_text_struct_frame_data : Maybe BattleTextStructFrame
        saved_battle_text_struct_frame_data =
            Nothing
    in
    { frame_edit_datas =
        { weapon =
            { form_definition = Magnolia.WeaponFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponFormUpdate)
            , frame_data = weapon_frame_data
            , saved_frame_data = saved_weapon_frame_data
            , table_view_page_info = Table.new_page_info (GotPageMsg WeaponFrame)
            }
        , armor =
            { form_definition = Magnolia.ArmorFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditArmorFormUpdate)
            , frame_data = armor_frame_data
            , saved_frame_data = saved_armor_frame_data
            , table_view_page_info = Table.new_page_info (GotPageMsg ArmorFrame)
            }
        , zone =
            { form_definition = Magnolia.ZoneFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditZoneFormUpdate)
            , frame_data = zone_frame_data
            , saved_frame_data = saved_zone_frame_data
            , table_view_page_info = Table.new_page_info (GotPageMsg ZoneFrame)
            }
        , weapon_category =
            { form_definition = Magnolia.WeaponCategoryFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponCategoryFormUpdate)
            , frame_data = weapon_category_frame_data
            , saved_frame_data = saved_weapon_category_frame_data
            , table_view_page_info = Table.new_page_info (GotPageMsg WeaponCategoryFrame)
            }
        , attribute =
            { form_definition = Magnolia.AttributeFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditAttributeFormUpdate)
            , frame_data = attribute_frame_data
            , saved_frame_data = saved_attribute_frame_data
            , table_view_page_info = Table.new_page_info (GotPageMsg AttributeFrame)
            }
        , battle_text_struct =
            { form_definition = Magnolia.BattleTextStructFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditBattleTextStructFormUpdate)
            , frame_data = battle_text_struct_frame_data
            , saved_frame_data = saved_battle_text_struct_frame_data
            , table_view_page_info = Table.new_page_info (GotPageMsg BattleTextStructFrame)
            }
        }
    , active_tab = Tab.customInitialState <| tab_prefix ++ "battle_text_struct_frame"
    , frame_view_mode = Edit
    }


update_frame_edit_data :
    FrameEditData frameData msg
    -> UpdateEditFormFunc frameData updateMsg
    -> updateMsg
    -> FrameEditData frameData msg
update_frame_edit_data frame_edit_data update_edit_form_data form_update_type =
    let
        frame_data =
            frame_edit_data.frame_data
    in
    { frame_edit_data | frame_data = update_edit_form_data frame_data form_update_type }


{-| takes a FrameEditDatas and a single FrameEditData and updates the Feds with the single one
-}
type alias FedsUpdater frameData msg =
    FrameEditDatas -> FrameEditData frameData msg -> FrameEditDatas


{-| returns a single Fed from a FrameEditDatas
-}
type alias FedGetter frameData msg =
    FrameEditDatas -> FrameEditData frameData msg


{-| takes a FrameData and a field update type (Name, Description) and returns the updated frame data
-}
type alias UpdateEditFormFunc frameData updateType =
    frameData -> updateType -> frameData


update_single_fed_frame_data : FrameEditData frameData msg -> frameData -> FrameEditData frameData msg
update_single_fed_frame_data fed new_frame_data =
    { fed | frame_data = new_frame_data }


update_feds : FrameEditDatas -> FedsUpdater frameData msg -> FrameEditData frameData msg -> FrameEditDatas
update_feds feds getter new_fed =
    getter feds new_fed


{-| updates the model's FrameEditDatas, using the UpdateEditFormFunc
-}
update_frame_edit_datas :
    Model
    -> FedGetter frameData msg
    -> FedsUpdater frameData msg
    -> UpdateEditFormFunc frameData updateType
    -> updateType
    -> Model
update_frame_edit_datas model fed_getter feds_updater update_edit_form_data form_update_type =
    let
        existing_feds =
            model.frame_edit_datas

        existing_fed =
            fed_getter existing_feds

        new_frame_data : frameData
        new_frame_data =
            update_edit_form_data existing_fed.frame_data form_update_type

        new_fed : FrameEditData frameData msg
        new_fed =
            update_single_fed_frame_data existing_fed new_frame_data
    in
    { model | frame_edit_datas = feds_updater existing_feds new_fed }

update_got_frame_edit_form_update : Model -> GotFrameEditFormUpdateMsg -> (Model, Cmd Msg)
update_got_frame_edit_form_update model sub_msg =
    case sub_msg of
        GotEditWeaponFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .weapon
                (\feds new_fed -> { feds | weapon = new_fed })
                Magnolia.WeaponFrame.update_edit_form_data
                form_update_type
            , Cmd.none
            )

        GotEditArmorFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .armor
                (\feds new_fed -> { feds | armor = new_fed })
                Magnolia.ArmorFrame.update_edit_form_data
                form_update_type
            , Cmd.none
            )

        GotEditZoneFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .zone
                (\feds new_fed -> { feds | zone = new_fed })
                Magnolia.ZoneFrame.update_edit_form_data
                form_update_type
            , Cmd.none
            )

        GotEditWeaponCategoryFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .weapon_category
                (\feds new_fed -> { feds | weapon_category = new_fed })
                Magnolia.WeaponCategoryFrame.update_edit_form_data
                form_update_type
            , Cmd.none
            )

        GotEditAttributeFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .attribute
                (\feds new_fed -> { feds | attribute = new_fed })
                Magnolia.AttributeFrame.update_edit_form_data
                form_update_type
            , Cmd.none
            )

        GotEditBattleTextStructFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .battle_text_struct
                (\feds new_fed -> { feds | battle_text_struct = new_fed })
                Magnolia.BattleTextStructFrame.update_edit_form_data
                form_update_type
            , Cmd.none
            )


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        ToggleFrameViewMode ->
            let
                new_frame_view_mode =
                    case model.frame_view_mode of
                        Edit ->
                            List

                        List ->
                            Edit
            in
            ( { model | frame_view_mode = new_frame_view_mode }, Cmd.none )

        GotFrameEditFormUpdate sub_msg ->
            update_got_frame_edit_form_update model sub_msg

        GotTabMsg new_state ->
            ( { model | active_tab = new_state }, Cmd.none )

        GotPageMsg frame_type page_msg ->
            let
                old_page_info =
                    get_page_info model.frame_edit_datas frame_type

                updated_page_info =
                    update_page_info old_page_info page_msg
            in
            ( { model | frame_edit_datas = set_page_info model.frame_edit_datas frame_type updated_page_info }, Cmd.none )


get_page_info : FrameEditDatas -> FrameType -> Table.PageInfo Msg
get_page_info feds frame_type =
    case frame_type of
        WeaponFrame ->
            feds.weapon.table_view_page_info

        ArmorFrame ->
            feds.armor.table_view_page_info

        ZoneFrame ->
            feds.zone.table_view_page_info

        WeaponCategoryFrame ->
            feds.weapon_category.table_view_page_info

        AttributeFrame ->
            feds.attribute.table_view_page_info

        BattleTextStructFrame ->
            feds.battle_text_struct.table_view_page_info


update_only_page_info : FrameEditData frameData Msg -> Table.PageInfo Msg -> FrameEditData frameData Msg
update_only_page_info old_fed new_page_info =
    { old_fed | table_view_page_info = new_page_info }


set_page_info : FrameEditDatas -> FrameType -> Table.PageInfo Msg -> FrameEditDatas
set_page_info feds frame_type new_page_info =
    case frame_type of
        WeaponFrame ->
            { feds | weapon = update_only_page_info feds.weapon new_page_info }

        ArmorFrame ->
            { feds | armor = update_only_page_info feds.armor new_page_info }

        ZoneFrame ->
            { feds | zone = update_only_page_info feds.zone new_page_info }

        WeaponCategoryFrame ->
            { feds | weapon_category = update_only_page_info feds.weapon_category new_page_info }

        AttributeFrame ->
            { feds | attribute = update_only_page_info feds.attribute new_page_info }

        BattleTextStructFrame ->
            { feds | battle_text_struct = update_only_page_info feds.battle_text_struct new_page_info }


bootstrap_button type_ on_click text_ =
    Button.button
        [ type_
        , Button.attrs [ onClick on_click ]
        ]
        [ text text_ ]


button_primary : msg -> String -> Html msg
button_primary on_click text_ =
    bootstrap_button Button.primary on_click text_


type alias TabItemConfig =
    { id : String, link_text : String, header : String, form_edit_view : Html Msg }


build_table_definition : List (FormData.FormField fd msg) -> TableDefinition fd
build_table_definition form_fields =
    { title = Just "Frame Table", columns = [] }


render_tab_item : Model -> TabItemConfig -> Tab.Item Msg
render_tab_item model config =
    let
        frame_edit_data : FrameEditData WeaponFrame Msg
        --TODO: make this generic
        frame_edit_data =
            model.frame_edit_datas.weapon

        --TODO: figure out how to get the form fields from this so i can build a table.
        --      maybe it'll be splitting up the form def again so the data is split somewhere
        form_def =
            -- Magnolia.WeaponFrame.edit_form_definition Magnolia.WeaponFrame.Name
            Magnolia.WeaponFrame.edit_form_definition GotEditWeaponFormUpdate

        form_fields =
            form_def.fields

        table_def : TableDefinition WeaponFrame
        table_def =
            build_table_definition form_fields

        row_data =
            [ frame_edit_data.frame_data ]

        page_info =
            Table.new_page_info <| GotPageMsg WeaponFrame

        rendered_tab_content =
            case model.frame_view_mode of
                Edit ->
                    config.form_edit_view

                List ->
                    Table.view table_def row_data frame_edit_data.table_view_page_info
    in
    Tab.item
        { id = config.id
        , link = Tab.link [] [ text config.link_text ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ h4 [] [ text config.header, button_primary ToggleFrameViewMode "Toggle View" ]
                , rendered_tab_content
                ]
        }


tabs_view : Model -> Html Msg
tabs_view model =
    let
        tab_configs : List TabItemConfig
        tab_configs =
            [ { id = tab_prefix ++ "weapon_frame", link_text = "WeaponFrame", header = "Edit WeaponFrame", form_edit_view = form_data_view model.frame_edit_datas.weapon }
            , { id = tab_prefix ++ "armor_frame", link_text = "ArmorFrame", header = "Edit ArmorFrame", form_edit_view = form_data_view model.frame_edit_datas.armor }
            , { id = tab_prefix ++ "zone_frame", link_text = "ZoneFrame", header = "Edit ZoneFrame", form_edit_view = form_data_view model.frame_edit_datas.zone }
            , { id = tab_prefix ++ "weapon_category_frame", link_text = "WeaponCategoryFrame", header = "Edit WeaponCategoryFrame", form_edit_view = form_data_view model.frame_edit_datas.weapon_category }
            , { id = tab_prefix ++ "attribute_frame", link_text = "AttributeFrame", header = "Edit AttributeFrame", form_edit_view = form_data_view model.frame_edit_datas.attribute }
            , { id = tab_prefix ++ "battle_text_struct_frame", link_text = "BattleTextStructFrame", header = "Edit BattleTextStructFrame", form_edit_view = form_data_view model.frame_edit_datas.battle_text_struct }
            ]

        tab_items =
            List.map (render_tab_item model) tab_configs
    in
    Tab.config GotTabMsg
        |> Tab.items tab_items
        |> Tab.view model.active_tab


view : Model -> Html Msg
view model =
    div []
        [ tabs_view model
        ]


form_data_view : FrameEditData obj Msg -> Html Msg
form_data_view frame_edit_data =
    let
        { frame_data, form_definition, saved_frame_data } =
            frame_edit_data
    in
    Grid.row [ Row.centerMd ]
        [ Grid.col [ Col.sm11, Col.md8 ]
            [ Form.form []
                [ FormData.render_fields form_definition.fields frame_data
                ]
            ]
        ]
