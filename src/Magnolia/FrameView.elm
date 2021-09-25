module Magnolia.FrameView exposing (Model, Msg(..), OutMsg(..), init, update, view)

-- import Json.Encode exposing (string)
-- import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)

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
import Html.Lazy
import Http
import Json.Decode exposing (Decoder, at, field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import List
import Magnolia.ArmorFrame exposing (ArmorFrame)
import Magnolia.AttributeFrame exposing (AttributeFrame)
import Magnolia.BattleTextStructFrame exposing (BattleTextStructFrame)
import Magnolia.WeaponCategoryFrame exposing (WeaponCategoryFrame)
import Magnolia.WeaponFrame exposing (BattleRow(..), WeaponDamageType(..), WeaponFrame, battle_row_type_from_int, weapon_damage_type_from_int)
import Magnolia.ZoneFrame exposing (ZoneFrame)
import OpenDota.OpenDota as OpenDota
import PostData exposing (PostData)
import ReCase exposing (ReCase(..), recase)
import Reddit
import String
import Table exposing (ColumnDef, ColumnType(..), PageInfoMsg, TableDefinition, update_page_info, view)
import Task
import Time
import Url
import Utils exposing (add_class, clojure_json_server, root_json_server_url)
import Weather


type GotFrameEditFormUpdateMsg
    = GotEditWeaponFormUpdate Magnolia.WeaponFrame.EditFormUpdateType
    | GotEditZoneFormUpdate Magnolia.ZoneFrame.EditFormUpdateType
    | GotEditWeaponCategoryFormUpdate Magnolia.WeaponCategoryFrame.EditFormUpdateType
    | GotEditAttributeFormUpdate Magnolia.AttributeFrame.EditFormUpdateType
    | GotEditBattleTextStructFormUpdate Magnolia.BattleTextStructFrame.EditFormUpdateType
    | GotEditArmorFormUpdate Magnolia.ArmorFrame.EditFormUpdateType


type AllFramesDownloaded
    = DownloadedAllWeaponFrames (Result Http.Error (List WeaponFrame))
    | DownloadedAllArmorFrames (Result Http.Error (List ArmorFrame))
    | DownloadedAllZoneFrames (Result Http.Error (List ZoneFrame))
    | DownloadedAllWeaponCategoryFrames (Result Http.Error (List WeaponCategoryFrame))
    | DownloadedAllAttributeFrames (Result Http.Error (List AttributeFrame))
    | DownloadedAllBattleTextStructFrames (Result Http.Error (List BattleTextStructFrame))


type TableRowClickedFrameType
    = ClickedTableRowWeaponFrame FrameType WeaponFrame
    | ClickedTableRowArmorFrame FrameType ArmorFrame
    | ClickedTableRowZoneFrame FrameType ZoneFrame
    | ClickedTableRowWeaponCategoryFrame FrameType WeaponCategoryFrame
    | ClickedTableRowAttributeFrame FrameType AttributeFrame
    | ClickedTableRowBattleTextStructFrame FrameType BattleTextStructFrame


type Msg
    = ToggleFrameViewMode
    | SetFrameViewMode FrameViewMode
    | GotFrameEditFormUpdate FrameType GotFrameEditFormUpdateMsg
    | SubmitFrameEditForm
    | GotSubmittedFrameEditForm (Result Http.Error String)
    | DoDownloadAllFrames FrameType
      -- | DoDownloadWeaponFrames
    | GotDownloadedAllFrames AllFramesDownloaded
      -- | GotDownloadedWeaponFrames (Result Http.Error (List WeaponFrame))
    | GotPageMsg FrameType PageInfoMsg
    | GotTabMsg Tab.State
    | ClickChangeTab FrameType
    | HashUpdated String --hash
    | TableRowClicked TableRowClickedFrameType


type OutMsg
    = Noop
    | ToVisualOutput String


type alias FrameEditData f msg =
    { form_definition : FormData.FormDefinition f msg
    , all_frames : List f
    , frame_data : f
    , saved_frame_data : Maybe f
    , table_view_page_info : Table.PageInfo msg
    -- , frame_type_str : String -- "weapon", "zone", "weapon_category", etc
    , frame_id_getter : f -> String -- "frame_id", "id", etc. Zones use data_names pretty sure so this'll get hairy
    }


type FrameType
    = NoFrameTypeDEBUG
    | WeaponFrameType (FrameEditData WeaponFrame Msg)
    | ArmorFrameType (FrameEditData ArmorFrame Msg)
    | ZoneFrameType (FrameEditData ZoneFrame Msg)
    | WeaponCategoryFrameType (FrameEditData WeaponCategoryFrame Msg)
    | AttributeFrameType (FrameEditData AttributeFrame Msg)
    | BattleTextStructFrameType (FrameEditData BattleTextStructFrame Msg)


to_string : FrameType -> String
to_string frame_type =
    case frame_type of
        WeaponFrameType fed ->
            "WeaponFrame"

        ArmorFrameType fed ->
            "ArmorFrame"

        ZoneFrameType fed ->
            "ZoneFrame"

        WeaponCategoryFrameType fed ->
            "WeaponCategoryFrame"

        AttributeFrameType fed ->
            "AttributeFrame"

        BattleTextStructFrameType fed ->
            "BattleTextStructFrame"

        NoFrameTypeDEBUG -> Debug.todo "oh no" "NO FRAME TYPE DEBUG"

to_data_name : FrameType -> String
to_data_name frame_type =
    case frame_type of
        WeaponFrameType fed ->
            "weapon"

        ArmorFrameType fed ->
            "armor"

        ZoneFrameType fed ->
            "zone"

        WeaponCategoryFrameType fed ->
            "weapon_category"

        AttributeFrameType fed ->
            "attribute"

        BattleTextStructFrameType fed ->
            "battle_text_struct"

        NoFrameTypeDEBUG -> Debug.todo "oh jeez" "no_frame_type_debug"


-- from_data_name : String -> Maybe FrameType
-- from_data_name frame_type_str =
--     case frame_type_str of
--         "weapon" ->
--             Just WeaponFrame
--
--         "armor" ->
--             Just ArmorFrame
--
--         "zone" ->
--             Just ZoneFrame
--
--         "weapon_category" ->
--             Just WeaponCategoryFrame
--
--         "attribute" ->
--             Just AttributeFrame
--
--         "battle_text_struct" ->
--             Just BattleTextStructFrame
--
--         _ ->
--             Nothing
--

type alias FrameEditDatas =
    { weapon : FrameType
    , armor : FrameType
    , zone : FrameType
    , weapon_category : FrameType
    , attribute : FrameType
    , battle_text_struct : FrameType
    -- { weapon : WeaponFrameType
    -- , armor : ArmorFrameType
    -- , zone : ZoneFrameType
    -- , weapon_category : WeaponCategoryFrameType
    -- , attribute : AttributeFrameType
    -- , battle_text_struct : BattleTextStructFrameType
    }


type FrameViewMode
    = List
    | Edit


type alias Model =
    { frame_edit_datas : FrameEditDatas
    , frame_view_mode : FrameViewMode
    , active_tab : Tab.State
    , active_tab_frame_type : FrameType
    }


tab_prefix : String
tab_prefix =
    "frame_view_tab__"


frame_type_from_hash : Model -> String -> FrameType
frame_type_from_hash model hash =
    if String.startsWith tab_prefix hash then
        let
            suffix =
                String.dropLeft (String.length tab_prefix) hash
        in
        case suffix of
            "weapon_frame" ->
                model.frame_edit_datas.weapon

            "armor_frame" ->
                model.frame_edit_datas.armor

            "zone_frame" ->
                model.frame_edit_datas.zone

            "weapon_category_frame" ->
                model.frame_edit_datas.weapon_category

            "attribute_frame" ->
                model.frame_edit_datas.attribute

            "battle_text_struct_frame" ->
                model.frame_edit_datas.battle_text_struct

            _ ->
                Debug.log ("error: unknown frame type suffix: " ++ suffix) model.frame_edit_datas.weapon

    else
        model.frame_edit_datas.weapon


get_weapon_fed : FrameType -> Maybe (FrameEditData WeaponFrame Msg)
get_weapon_fed frame_type = case frame_type of
    WeaponFrameType fed -> Just fed
    _ -> Nothing

get_armor_fed : FrameType -> Maybe (FrameEditData ArmorFrame Msg)
get_armor_fed frame_type = case frame_type of
    ArmorFrameType fed -> Just fed
    _ -> Nothing

get_zone_fed : FrameType -> Maybe (FrameEditData ZoneFrame Msg)
get_zone_fed frame_type = case frame_type of
    ZoneFrameType fed -> Just fed
    _ -> Nothing

get_weapon_category_fed : FrameType -> Maybe (FrameEditData WeaponCategoryFrame Msg)
get_weapon_category_fed frame_type = case frame_type of
    WeaponCategoryFrameType fed -> Just fed
    _ -> Nothing

get_attribute_fed : FrameType -> Maybe (FrameEditData AttributeFrame Msg)
get_attribute_fed frame_type = case frame_type of
    AttributeFrameType fed -> Just fed
    _ -> Nothing

get_battle_text_struct_fed : FrameType -> Maybe (FrameEditData BattleTextStructFrame Msg)
get_battle_text_struct_fed frame_type = case frame_type of
    BattleTextStructFrameType fed -> Just fed
    _ -> Nothing

init : String -> ( Model, Cmd Msg )
init hash =
    let
        weapon_frame_data : WeaponFrame
        weapon_frame_data =
            { frame_id = 123
            , pretty_name = "Init Pretty Name"
            , description = "init description"
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
            { frame_id = 123
            , pretty_name = "Unset ArmorFrame Name"
            , description = "unset desc"
            , frame_image_path = "armor_img.png"
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
            , battle_won_text_fid = -1
            , battle_tied_text_fid = -1
            , battle_lost_text_fid = -1
            }

        saved_battle_text_struct_frame_data : Maybe BattleTextStructFrame
        saved_battle_text_struct_frame_data =
            Nothing

        initial_active_tab =
            case hash of
                "" ->
                    Tab.customInitialState <| tab_prefix ++ "weapon_frame"

                _ ->
                    Tab.customInitialState <| hash

        temp_handler : Table.PageInfoMsg -> Msg
        temp_handler _ = Debug.todo "Implement this once init_model is initialized" ToggleFrameViewMode

        temp_fields : FormData.FormDefinition f msg
        temp_fields = {fields = []}

        weapon_frame_type : FrameType
        weapon_frame_type = WeaponFrameType
                    -- { form_definition = Magnolia.WeaponFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponFormUpdate)
                    { form_definition = temp_fields
                    , frame_data = weapon_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_weapon_frame_data
                    , table_view_page_info = Table.new_page_info temp_handler
                    , frame_id_getter = String.fromInt << .frame_id
                    }

        -- hack to replace the page info, since it requires a full frame_type
        replace_page_info : FrameType -> FrameType
        replace_page_info frame_type =
                let
                    new_page_info = Table.new_page_info (GotPageMsg frame_type)
                in
                    case frame_type of
                        WeaponFrameType fed -> WeaponFrameType {fed | table_view_page_info = new_page_info }
                        ArmorFrameType fed -> ArmorFrameType {fed | table_view_page_info = new_page_info }
                        ZoneFrameType fed -> ZoneFrameType {fed | table_view_page_info = new_page_info }
                        WeaponCategoryFrameType fed -> WeaponCategoryFrameType {fed | table_view_page_info = new_page_info }
                        AttributeFrameType fed -> AttributeFrameType {fed | table_view_page_info = new_page_info }
                        BattleTextStructFrameType fed -> BattleTextStructFrameType {fed | table_view_page_info = new_page_info }
                        _ -> Debug.todo "remove NOFRAMETYPE" frame_type

        -- hack to replace the form definitions, since it requires a full frame_type
        replace_form_def :
                        (msg -> FormData.FormDefinition fd msg)
                        -> FrameType
                        -> FrameType
        replace_form_def edit_form frame_type =
                let
                      -- new_form_def = edit_form (GotFrameEditFormUpdate frame_type)
                      the_msg = (\sub_msg -> GotFrameEditFormUpdate frame_type sub_msg)
                      _ = 1
                in
                    case frame_type of
                        WeaponFrameType fed -> WeaponFrameType {fed | form_definition = Magnolia.WeaponFrame.edit_form_definition (the_msg << GotEditWeaponFormUpdate) }
                        ArmorFrameType fed -> ArmorFrameType {fed | form_definition = Magnolia.ArmorFrame.edit_form_definition (the_msg << GotEditArmorFormUpdate) }
                        ZoneFrameType fed -> ZoneFrameType {fed | form_definition = Magnolia.ZoneFrame.edit_form_definition (the_msg << GotEditZoneFormUpdate) }
                        WeaponCategoryFrameType fed -> WeaponCategoryFrameType {fed | form_definition = Magnolia.WeaponCategoryFrame.edit_form_definition (the_msg << GotEditWeaponCategoryFormUpdate) }
                        AttributeFrameType fed -> AttributeFrameType {fed | form_definition = Magnolia.AttributeFrame.edit_form_definition (the_msg << GotEditAttributeFormUpdate) }
                        BattleTextStructFrameType fed -> BattleTextStructFrameType {fed | form_definition = Magnolia.BattleTextStructFrame.edit_form_definition (the_msg << GotEditBattleTextStructFormUpdate) }
                        _ -> Debug.todo "remove NOFRAMETYPE" frame_type


        init_model : Model
        init_model =
            { frame_edit_datas =
                { weapon = replace_form_def Magnolia.WeaponFrame.edit_form_definition <| replace_page_info weapon_frame_type
                , armor = replace_page_info <| ArmorFrameType
                    { form_definition = Magnolia.ArmorFrame.edit_form_definition (GotFrameEditFormUpdate NoFrameTypeDEBUG << GotEditArmorFormUpdate)
                    , frame_data = armor_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_armor_frame_data
                    , table_view_page_info = Table.new_page_info temp_handler
                    , frame_id_getter = String.fromInt << .frame_id
                    }
                , zone = replace_page_info <| ZoneFrameType
                    { form_definition = Magnolia.ZoneFrame.edit_form_definition (GotFrameEditFormUpdate NoFrameTypeDEBUG << GotEditZoneFormUpdate)
                    , frame_data = zone_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_zone_frame_data
                    , table_view_page_info = Table.new_page_info temp_handler
                    , frame_id_getter = .data_name
                    }
                , weapon_category = replace_page_info <| WeaponCategoryFrameType
                    { form_definition = Magnolia.WeaponCategoryFrame.edit_form_definition (GotFrameEditFormUpdate NoFrameTypeDEBUG << GotEditWeaponCategoryFormUpdate)
                    , frame_data = weapon_category_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_weapon_category_frame_data
                    , table_view_page_info = Table.new_page_info temp_handler
                    , frame_id_getter = String.fromInt << .frame_id
                    }
                , attribute = replace_page_info <| AttributeFrameType
                    { form_definition = Magnolia.AttributeFrame.edit_form_definition (GotFrameEditFormUpdate NoFrameTypeDEBUG << GotEditAttributeFormUpdate)
                    , frame_data = attribute_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_attribute_frame_data
                    , table_view_page_info = Table.new_page_info temp_handler
                    , frame_id_getter = String.fromInt << .frame_id
                    }
                , battle_text_struct = replace_page_info <| BattleTextStructFrameType
                    { form_definition = Magnolia.BattleTextStructFrame.edit_form_definition (GotFrameEditFormUpdate NoFrameTypeDEBUG << GotEditBattleTextStructFormUpdate)
                    , frame_data = battle_text_struct_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_battle_text_struct_frame_data
                    , table_view_page_info = Table.new_page_info temp_handler
                    , frame_id_getter = String.fromInt << .frame_id
                    }
                }
            , active_tab = initial_active_tab
            , active_tab_frame_type = weapon_frame_type
            -- , active_tab_frame_type = initial_active_tab_frame_type

            , frame_view_mode = List
            -- , frame_view_mode = Edit
            }

        initial_active_tab_frame_type : FrameType
        initial_active_tab_frame_type =
            frame_type_from_hash init_model hash

        finalized_model : Model
        finalized_model = { init_model |
            active_tab_frame_type = initial_active_tab_frame_type }

        -- init_cmds = Cmd.none
        init_cmds =
            Cmd.batch
                [-- Task.perform (\_ -> DoDownloadAllFrames initial_active_tab_frame_type) Time.now
                ]
    in
    ( finalized_model, init_cmds )


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
-- type alias FedsUpdater frameData msg =
type alias FedsUpdater =
    FrameEditDatas -> FrameType -> FrameEditDatas


{-| returns a single Fed from a FrameEditDatas
-}
type alias FedGetter =
    FrameEditDatas -> FrameType


{-| takes a FrameData and a field update type (Name, Description) and returns the updated frame data
-}
type alias UpdateEditFormFunc frameData updateType =
    frameData -> updateType -> frameData


-- update_single_fed_frame_data : FrameEditData frameData msg -> frameData -> FrameEditData frameData msg
-- update_single_fed_frame_data fed new_frame_data =
-- update_single_fed_frame_data : FrameType -> frameData -> FrameType
-- update_single_fed_frame_data frame_type new_frame_data =
--     case frame_type of
--         WeaponFrameType old_fed -> WeaponFrameType new_frame_data
--         ArmorFrameType old_fed -> ArmorFrameType new_frame_data
--         ZoneFrameType old_fed -> ZoneFrameType new_frame_data
--         WeaponCategoryFrameType old_fed -> WeaponCategoryFrameType new_frame_data
--         AttributeFrameType old_fed -> AttributeFrameType new_frame_data
--         BattleTextStructFrameType old_fed -> BattleTextStructFrameType new_frame_data


-- update_feds : FrameEditDatas -> FedsUpdater -> FrameEditData frameData msg -> FrameEditDatas
-- update_feds feds feds_updater new_fed =
--     feds_updater feds new_fed
--

{-| updates the model's FrameEditDatas, using the UpdateEditFormFunc
-}
-- update_frame_edit_datas :
--     Model
--     -> FedGetter
--     -> FedsUpdater
--     -> UpdateEditFormFunc frameData updateType
--     -> updateType
--     -> Model
-- update_frame_edit_datas model fed_getter feds_updater update_edit_form_data form_update_type =
--     let
--         existing_feds : FrameEditDatas
--         existing_feds =
--             model.frame_edit_datas
--
--         existing_fed : FrameEditData frameData msg
--         existing_fed =
--             fed_getter existing_feds
--
--         new_frame_data : frameData
--         new_frame_data =
--             update_edit_form_data existing_fed.frame_data form_update_type
--
--         new_fed : FrameEditData frameData msg
--         new_fed =
--             update_single_fed_frame_data existing_fed new_frame_data
--     in
--     { model | frame_edit_datas = feds_updater existing_feds new_fed }


update_got_frame_download_all_frames_update : Model -> Msg -> ( Model, Cmd Msg )
update_got_frame_download_all_frames_update model sub_msg =
    ( model, Cmd.none )


update_got_frame_edit_form_update : Model -> FrameType -> GotFrameEditFormUpdateMsg -> ( Model, Cmd Msg )
update_got_frame_edit_form_update model frame_type sub_msg =
    case sub_msg of
        GotEditWeaponFormUpdate form_update_type ->
            -- ( update_frame_edit_datas
            --     model
            --     .weapon
            --     update_fed_weapon
            --     Magnolia.WeaponFrame.update_edit_form_data
            --     form_update_type
            -- , Cmd.none
            -- )

            let
                feds = model.frame_edit_datas
                existing_frame_type = feds.weapon
                updated_frame_type = case get_weapon_fed feds.weapon of
                    Just fed_ ->
                                    let
                                        frame_data = fed_.frame_data
                                    in
                                        Just <| WeaponFrameType {fed_ | frame_data = Magnolia.WeaponFrame.update_edit_form_data frame_data form_update_type}
                    Nothing -> Nothing

                new_feds = case updated_frame_type of
                    Just new_frame_type -> {feds | weapon = new_frame_type}
                    Nothing -> feds
            in

            ( {model | frame_edit_datas = new_feds}

            , Cmd.none)

        _ -> Debug.todo "handle all GotEditFRAMEFormUpdate" (model, Cmd.none)

        -- GotEditArmorFormUpdate form_update_type ->
        --     ( update_frame_edit_datas
        --         model
        --         .armor
        --         update_fed_armor
        --         Magnolia.ArmorFrame.update_edit_form_data
        --         form_update_type
        --     , Cmd.none
        --     )
        --
        -- GotEditZoneFormUpdate form_update_type ->
        --     ( update_frame_edit_datas
        --         model
        --         .zone
        --         update_fed_zone
        --         Magnolia.ZoneFrame.update_edit_form_data
        --         form_update_type
        --     , Cmd.none
        --     )
        --
        -- GotEditWeaponCategoryFormUpdate form_update_type ->
        --     ( update_frame_edit_datas
        --         model
        --         .weapon_category
        --         update_fed_weapon_category
        --         Magnolia.WeaponCategoryFrame.update_edit_form_data
        --         form_update_type
        --     , Cmd.none
        --     )
        --
        -- GotEditAttributeFormUpdate form_update_type ->
        --     ( update_frame_edit_datas
        --         model
        --         .attribute
        --         update_fed_attribute
        --         Magnolia.AttributeFrame.update_edit_form_data
        --         form_update_type
        --     , Cmd.none
        --     )
        --
        -- GotEditBattleTextStructFormUpdate form_update_type ->
        --     ( update_frame_edit_datas
        --         model
        --         .battle_text_struct
        --         update_fed_battle_text_struct
        --         Magnolia.BattleTextStructFrame.update_edit_form_data
        --         form_update_type
        --     , Cmd.none
        --     )
        --

unpack_response : Result Http.Error a -> Maybe a
unpack_response response =
    case response of
        Ok values ->
            Just values

        Err err ->
            let
                _ =
                    Debug.log "Error: \n" err
            in
            Nothing

-- generate_new_frame_type : FrameType -> FrameEditData frameData msg -> FrameType
-- generate_new_frame_type old_frame_type new_fed =
--     Debug.todo "fix this doing nothing" <|  case old_frame_type of
--         WeaponFrameType fed -> WeaponFrameType fed
--         ArmorFrameType fed -> ArmorFrameType fed
--         ZoneFrameType fed -> ZoneFrameType fed
--         WeaponCategoryFrameType fed -> WeaponCategoryFrameType fed
--         AttributeFrameType fed -> AttributeFrameType fed
--         BattleTextStructFrameType fed -> BattleTextStructFrameType fed


-- get_frame_data : FrameType -> FrameEditData frameData msg
-- get_frame_data frame_type =
--     case frame_type of
--         WeaponFrameType fed -> fed
--         ArmorFrameType fed -> fed
--         ZoneFrameType fed -> fed
--         WeaponCategoryFrameType fed -> fed
--         AttributeFrameType fed -> fed
--         BattleTextStructFrameType fed -> fed

update_existing_fed existing_fed all_frames new_page_info =
    { existing_fed
        | all_frames = all_frames
        , table_view_page_info = new_page_info
    }


-- handle_feds_download :
--     FrameEditDatas
--     -> FedGetter
--     -> FedsUpdater
--     -> Maybe (List frameData)
--     -> FrameEditDatas
-- handle_feds_download existing_feds fed_getter feds_updater maybe_all_frames =
--     let
--         existing_frame_type : FrameType
--         existing_frame_type =
--             fed_getter existing_feds
--
--         -- all_frames : List frameData
--         all_frames =
--             case maybe_all_frames of
--                 Just new_all_frames ->
--                     new_all_frames
--
--                 Nothing ->
--                     -- existing_fed.all_frames
--                     [] --TODO reuse existing all_frames from existing_fed now that they're FrameType
--
--         new_page_info =
--             Table.initialize_page_info (get_page_info existing_frame_type) all_frames
--
--
--         -- existing_fed : FrameEditData frameData Msg
--         -- existing_fed = get_frame_data existing_frame_type
--         -- new_fed = case existing_frame_type of
--         --                     WeaponFrameType fed -> update_existing_fed fed all_frames new_page_info
--         --                     ArmorFrameType fed -> update_existing_fed fed all_frames new_page_info
--         --                     ZoneFrameType fed -> update_existing_fed fed all_frames new_page_info
--         --                     WeaponCategoryFrameType fed -> update_existing_fed fed all_frames new_page_info
--         --                     AttributeFrameType fed -> update_existing_fed fed all_frames new_page_info
--         --                     BattleTextStructFrameType fed -> update_existing_fed fed all_frames new_page_info
--
--         -- new_fed : FrameEditData frameData msg
--         -- new_fed =
--         --     update_existing_fed existing_fed all_frames new_page_info
--
--         new_frame_type : FrameType
--         new_frame_type = case existing_frame_type of
--                             WeaponFrameType fed -> WeaponFrameType <| update_existing_fed fed all_frames new_page_info
--                             ArmorFrameType fed -> ArmorFrameType <| update_existing_fed fed all_frames new_page_info
--                             ZoneFrameType fed -> ZoneFrameType <| update_existing_fed fed all_frames new_page_info
--                             WeaponCategoryFrameType fed -> WeaponCategoryFrameType <| update_existing_fed fed all_frames new_page_info
--                             AttributeFrameType fed -> AttributeFrameType <| update_existing_fed fed all_frames new_page_info
--                             BattleTextStructFrameType fed -> BattleTextStructFrameType <| update_existing_fed fed all_frames new_page_info
--     in
--     feds_updater existing_feds new_frame_type
--

update_do_download_all_frames : Model -> FrameType -> ( Model, Cmd Msg )
update_do_download_all_frames model frame_type =
    case frame_type of
        WeaponFrameType fed ->
            ( model, Magnolia.WeaponFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllWeaponFrames) )

        ArmorFrameType fed ->
            ( model, Magnolia.ArmorFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllArmorFrames) )

        ZoneFrameType fed ->
            ( model, Magnolia.ZoneFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllZoneFrames) )

        WeaponCategoryFrameType fed ->
            ( model, Magnolia.WeaponCategoryFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllWeaponCategoryFrames) )

        AttributeFrameType fed ->
            ( model, Magnolia.AttributeFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllAttributeFrames) )

        BattleTextStructFrameType fed ->
            ( model, Magnolia.BattleTextStructFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllBattleTextStructFrames) )

        NoFrameTypeDEBUG -> Debug.todo "oh no" (model, Cmd.none)



-- _ ->
--     Debug.todo "ASDASDSADSDSDS\n\nasdsad" ( model, Cmd.none )


update_got_downloaded_all_frames : Model -> AllFramesDownloaded -> ( Model, Cmd Msg )
update_got_downloaded_all_frames model sub_msg =
    let
        feds =
            model.frame_edit_datas

        cmd =
            Cmd.none

        new_feds =
            case sub_msg of
                DownloadedAllWeaponFrames response ->
                    let
                        maybe_all_frames = unpack_response response
                        new_frame_type = case feds.weapon of
                            WeaponFrameType existing_fed -> WeaponFrameType {existing_fed | all_frames = Maybe.withDefault existing_fed.all_frames maybe_all_frames}
                            _ -> feds.weapon
                    in
                    {feds | weapon =  new_frame_type}

                DownloadedAllArmorFrames response ->
                    let
                        maybe_all_frames = unpack_response response
                        new_frame_type = case feds.armor of
                            ArmorFrameType existing_fed -> ArmorFrameType {existing_fed | all_frames = Maybe.withDefault existing_fed.all_frames maybe_all_frames}
                            _ -> feds.armor
                    in
                    {feds | armor =  new_frame_type}

                DownloadedAllZoneFrames response ->
                    let
                        maybe_all_frames = unpack_response response
                        new_frame_type = case feds.zone of
                            ZoneFrameType existing_fed -> ZoneFrameType {existing_fed | all_frames = Maybe.withDefault existing_fed.all_frames maybe_all_frames}
                            _ -> feds.zone
                    in
                    {feds | zone =  new_frame_type}

                DownloadedAllWeaponCategoryFrames response ->
                    let
                        maybe_all_frames = unpack_response response
                        new_frame_type = case feds.weapon_category of
                            WeaponCategoryFrameType existing_fed -> WeaponCategoryFrameType {existing_fed | all_frames = Maybe.withDefault existing_fed.all_frames maybe_all_frames}
                            _ -> feds.weapon_category
                    in
                    {feds | weapon_category =  new_frame_type}

                DownloadedAllAttributeFrames response ->
                    let
                        maybe_all_frames = unpack_response response
                        new_frame_type = case feds.attribute of
                            AttributeFrameType existing_fed -> AttributeFrameType {existing_fed | all_frames = Maybe.withDefault existing_fed.all_frames maybe_all_frames}
                            _ -> feds.attribute
                    in
                    {feds | attribute =  new_frame_type}

                DownloadedAllBattleTextStructFrames response ->
                    let
                        maybe_all_frames = unpack_response response
                        new_frame_type = case feds.battle_text_struct of
                            BattleTextStructFrameType existing_fed -> BattleTextStructFrameType {existing_fed | all_frames = Maybe.withDefault existing_fed.all_frames maybe_all_frames}
                            _ -> feds.battle_text_struct
                    in
                    {feds | battle_text_struct =  new_frame_type}


    in
    ( { model | frame_edit_datas = new_feds }, cmd )


update_fed_weapon : FrameEditDatas -> FrameEditData WeaponFrame Msg -> FrameEditDatas
update_fed_weapon feds new_fed =
    { feds | weapon = WeaponFrameType new_fed }


update_fed_armor : FrameEditDatas -> FrameEditData ArmorFrame Msg -> FrameEditDatas
update_fed_armor feds new_fed =
    { feds | armor = ArmorFrameType new_fed }


update_fed_zone : FrameEditDatas -> FrameEditData ZoneFrame Msg -> FrameEditDatas
update_fed_zone feds new_fed =
    { feds | zone = ZoneFrameType new_fed }


update_fed_weapon_category : FrameEditDatas -> FrameEditData WeaponCategoryFrame Msg -> FrameEditDatas
update_fed_weapon_category feds new_fed =
    { feds | weapon_category = WeaponCategoryFrameType new_fed }


update_fed_attribute : FrameEditDatas -> FrameEditData AttributeFrame Msg -> FrameEditDatas
update_fed_attribute feds new_fed =
    { feds | attribute = AttributeFrameType new_fed }


update_fed_battle_text_struct : FrameEditDatas -> FrameEditData BattleTextStructFrame Msg -> FrameEditDatas
update_fed_battle_text_struct feds new_fed =
    { feds | battle_text_struct = BattleTextStructFrameType new_fed }


update_table_row_clicked_frame_type : Model -> TableRowClickedFrameType -> ( Model, Cmd Msg )
update_table_row_clicked_frame_type model sub_msg =
    let
        feds =
            model.frame_edit_datas

        new_feds =
            case sub_msg of
                ClickedTableRowWeaponFrame frame_type frame_data ->
                    case feds.weapon of
                        WeaponFrameType fed ->
                            { feds | weapon =
                                WeaponFrameType {fed | frame_data = frame_data}
                            }
                        _ -> feds


                _ -> Debug.todo "one more" feds

                -- ClickedTableRowArmorFrame frame_type frame_data ->
                --     update_fed_armor feds <|
                --         update_single_fed_frame_data feds.armor frame_data
                --
                -- ClickedTableRowZoneFrame frame_type frame_data ->
                --     update_fed_zone feds <|
                --         update_single_fed_frame_data feds.zone frame_data
                --
                -- ClickedTableRowWeaponCategoryFrame frame_type frame_data ->
                --     update_fed_weapon_category feds <|
                --         update_single_fed_frame_data feds.weapon_category frame_data
                --
                -- ClickedTableRowAttributeFrame frame_type frame_data ->
                --     update_fed_attribute feds <|
                --         update_single_fed_frame_data feds.attribute frame_data
                --
                -- ClickedTableRowBattleTextStructFrame frame_type frame_data ->
                --     update_fed_battle_text_struct feds <|
                --         update_single_fed_frame_data feds.battle_text_struct frame_data

        -- toggle the ui so we are looking at the detail mode
        ( new_model, new_cmd, _ ) =
            update { model | frame_edit_datas = new_feds } ToggleFrameViewMode
    in
    ( new_model, new_cmd )


update : Model -> Msg -> ( Model, Cmd Msg, OutMsg )
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
            ( { model | frame_view_mode = new_frame_view_mode }, Cmd.none, Noop )

        SetFrameViewMode new_frame_view_mode ->
            ( { model | frame_view_mode = new_frame_view_mode }, Cmd.none, Noop )

        GotFrameEditFormUpdate frame_type sub_msg ->
            let
                ( model_, cmd ) =
                    update_got_frame_edit_form_update model frame_type sub_msg
            in
            ( model_, cmd, Noop )

        SubmitFrameEditForm ->
            -- TODO: make request to clojure server to update frame
            let
                active_frame_type =
                    model.active_tab_frame_type

                url_suffix =
                    "api/frames/"
                        ++ to_data_name active_frame_type

                cmd =
                    Debug.log "submitting"
                        Http.get
                        { url = clojure_json_server ++ url_suffix
                        , expect = Http.expectString GotSubmittedFrameEditForm
                        }
            in
            ( model, cmd, Noop )

        GotSubmittedFrameEditForm resp ->
            let
                val =
                    Debug.log "resp came through" <|
                        case resp of
                            Ok str_ ->
                                str_

                            Err err ->
                                ""
            in
            ( model, Cmd.none, Noop )

        -- DoDownloadWeaponFrames ->
        DoDownloadAllFrames frame_type ->
            let
                ( model_, cmd ) =
                    update_do_download_all_frames model frame_type
            in
            ( model_, cmd, Noop )

        -- GotDownloadedWeaponFrames response ->
        GotDownloadedAllFrames all_frames_downloaded ->
            let
                ( new_model, new_cmd ) =
                    update_got_downloaded_all_frames model all_frames_downloaded
            in
            ( new_model, new_cmd, Noop )

        GotTabMsg new_active_tab ->
            let
                _ =
                    Debug.log "changing tabs" ""

                ( new_model, new_cmd, _ ) =
                    update { model | active_tab = new_active_tab } <| SetFrameViewMode List
            in
            ( new_model, new_cmd, Noop )

        ClickChangeTab frame_type ->
            ( model, Cmd.none, Noop )

        HashUpdated hash ->
            let
                new_frame_type =
                    frame_type_from_hash model hash
            in
            ( { model | active_tab_frame_type = new_frame_type }, Task.perform (\_ -> DoDownloadAllFrames new_frame_type) Time.now, Noop )

        GotPageMsg frame_type page_msg ->
            let
                old_page_info =
                    get_page_info frame_type

                updated_page_info =
                    update_page_info old_page_info page_msg
            in
            ( { model | frame_edit_datas = set_page_info model.frame_edit_datas frame_type updated_page_info }, Cmd.none, Noop )

        TableRowClicked sub_msg ->
            let
                ( model_, cmd ) =
                    update_table_row_clicked_frame_type model sub_msg
            in
            -- ( model_, cmd, ToVisualOutput "Clicked a table row")
            ( model_, cmd, Noop )


get_page_info : FrameType -> Table.PageInfo Msg
get_page_info frame_type =
    case frame_type of
        WeaponFrameType fed ->
            fed.table_view_page_info

        ArmorFrameType fed ->
            fed.table_view_page_info

        ZoneFrameType fed ->
            fed.table_view_page_info

        WeaponCategoryFrameType fed ->
            fed.table_view_page_info

        AttributeFrameType fed ->
            fed.table_view_page_info

        BattleTextStructFrameType fed ->
            fed.table_view_page_info

        NoFrameTypeDEBUG -> Debug.todo "oh no" "NO FRAME TYPE DEBUG"


update_only_page_info : FrameEditData frameData Msg -> Table.PageInfo Msg -> FrameEditData frameData Msg
update_only_page_info old_fed new_page_info =
    { old_fed | table_view_page_info = new_page_info }


set_page_info : FrameEditDatas -> FrameType -> Table.PageInfo Msg -> FrameEditDatas
set_page_info feds frame_type new_page_info =
    case frame_type of
        WeaponFrameType fed ->
            update_fed_weapon feds <| update_only_page_info fed new_page_info

        ArmorFrameType fed ->
            update_fed_armor feds <| update_only_page_info fed new_page_info

        ZoneFrameType fed ->
            update_fed_zone feds <| update_only_page_info fed new_page_info

        WeaponCategoryFrameType fed ->
            update_fed_weapon_category feds <| update_only_page_info fed new_page_info

        AttributeFrameType fed ->
            update_fed_attribute feds <| update_only_page_info fed new_page_info

        BattleTextStructFrameType fed ->
            update_fed_battle_text_struct feds <| update_only_page_info fed new_page_info

        NoFrameTypeDEBUG -> Debug.todo "oh no fallback" feds


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
    { form_edit_view : Html Msg, frame_type : FrameType }


build_table_definition : FrameType -> List (FormData.FormField fd Msg) -> (fd -> Msg) -> TableDefinition fd Msg
build_table_definition frame_type form_fields on_row_click =
    { title = Just <| to_string frame_type
    , columns = List.indexedMap form_field_to_column form_fields
    , on_row_click = on_row_click
    }


{-| Looks up a FrameData's field and renders to a string
-}
field_lookup : FormData.FormField fd msg -> fd -> String
field_lookup field obj =
    case field.data_type of
        IntType getter ->
            String.fromInt <| getter obj

        FloatType getter ->
            String.fromFloat <| getter obj

        StringType getter ->
            getter obj

        EnumType getter ->
            getter obj

        ListStringType getter ->
            String.join ", " <| getter obj

        ListIntType getter ->
            String.join ", " <| List.map String.fromInt <| getter obj


form_field_to_column : Int -> FormData.FormField fd msg -> ColumnDef fd
form_field_to_column idx form_field =
    { column_id = form_field.field_name
    , idx = idx
    , pretty_title = form_field.field_name
    , styles = []
    , lookup_func = field_lookup form_field
    , column_type = String --TODO
    }


render_tab_item :
    Model
    -> TabItemConfig
    -> FrameEditData fd Msg
    -> FormData.FormDefinition fd Msg
    -> (fd -> Msg)
    -> Tab.Item Msg
render_tab_item model config frame_edit_data form_definition on_row_click =
    let
        frame_type : FrameType
        frame_type =
            config.frame_type

        form_fields : List (FormData.FormField fd Msg)
        form_fields =
            form_definition.fields

        table_definition : TableDefinition fd Msg
        table_definition =
            build_table_definition frame_type form_fields on_row_click

        row_data : List fd
        row_data =
            frame_edit_data.all_frames

        page_info : Table.PageInfo Msg
        page_info =
            Table.new_page_info <| GotPageMsg frame_type

        rendered_tab_content : Html Msg
        rendered_tab_content =
            case model.frame_view_mode of
                Edit ->
                    config.form_edit_view

                List ->
                    Table.view table_definition row_data frame_edit_data.table_view_page_info

        tab_id =
            tab_prefix ++ (recase ToSnake <| to_string frame_type)
    in
    Tab.item
        { id = tab_id
        , link = Tab.link [] [ text <| to_string frame_type ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ Button.button
                    [ Button.outlineSecondary
                    , Button.block
                    , Button.attrs [ onClick ToggleFrameViewMode ]
                    ]
                    [ text "Full Toggle View" ]
                , br [] []
                , rendered_tab_content
                ]
        }


do_render_tab : Model -> TabItemConfig -> Tab.Item Msg
do_render_tab model config =
    case config.frame_type of
        WeaponFrameType fed ->
            render_tab_item model
                config
                fed
                (Magnolia.WeaponFrame.edit_form_definition (GotFrameEditFormUpdate config.frame_type << GotEditWeaponFormUpdate))
                (TableRowClicked << ClickedTableRowWeaponFrame (WeaponFrameType fed ))

        ArmorFrameType fed ->
            render_tab_item model
                config
                fed
                (Magnolia.ArmorFrame.edit_form_definition (GotFrameEditFormUpdate config.frame_type << GotEditArmorFormUpdate))
                (TableRowClicked << ClickedTableRowArmorFrame (ArmorFrameType fed ))

        ZoneFrameType fed ->
            render_tab_item model
                config
                fed
                (Magnolia.ZoneFrame.edit_form_definition (GotFrameEditFormUpdate config.frame_type << GotEditZoneFormUpdate))
                (TableRowClicked << ClickedTableRowZoneFrame (ZoneFrameType fed ))

        WeaponCategoryFrameType fed ->
            render_tab_item model
                config
                fed
                (Magnolia.WeaponCategoryFrame.edit_form_definition (GotFrameEditFormUpdate config.frame_type << GotEditWeaponCategoryFormUpdate))
                (TableRowClicked << ClickedTableRowWeaponCategoryFrame (WeaponCategoryFrameType fed))

        AttributeFrameType fed ->
            render_tab_item model
                config
                fed
                (Magnolia.AttributeFrame.edit_form_definition (GotFrameEditFormUpdate config.frame_type << GotEditAttributeFormUpdate))
                (TableRowClicked << ClickedTableRowAttributeFrame (AttributeFrameType fed))

        BattleTextStructFrameType fed ->
            render_tab_item model
                config
                fed
                (Magnolia.BattleTextStructFrame.edit_form_definition (GotFrameEditFormUpdate config.frame_type << GotEditBattleTextStructFormUpdate))
                (TableRowClicked << ClickedTableRowBattleTextStructFrame (BattleTextStructFrameType fed))

        NoFrameTypeDEBUG -> Debug.todo "oh no" "NO FRAME TYPE DEBUG"

wrap_form_data_view : FrameType -> Html Msg
wrap_form_data_view frame_type =
    case frame_type of
        WeaponFrameType fed -> form_data_view fed
        ArmorFrameType fed -> form_data_view fed
        ZoneFrameType fed -> form_data_view fed
        WeaponCategoryFrameType fed -> form_data_view fed
        AttributeFrameType fed -> form_data_view fed
        BattleTextStructFrameType fed -> form_data_view fed
        NoFrameTypeDEBUG -> Debug.todo "oh no" "NO FRAME TYPE DEBUG"

tabs_view : Model -> Html Msg
tabs_view model =
    let
        tab_configs : List TabItemConfig
        tab_configs =
            [ { form_edit_view = wrap_form_data_view model.frame_edit_datas.weapon
              , frame_type = model.frame_edit_datas.weapon
              }
            , { form_edit_view = wrap_form_data_view model.frame_edit_datas.armor
              , frame_type = model.frame_edit_datas.armor
              }
            , { form_edit_view = wrap_form_data_view model.frame_edit_datas.zone
              , frame_type = model.frame_edit_datas.zone
              }
            , { form_edit_view = wrap_form_data_view model.frame_edit_datas.weapon_category
              , frame_type = model.frame_edit_datas.weapon_category
              }
            , { form_edit_view = wrap_form_data_view model.frame_edit_datas.attribute
              , frame_type = model.frame_edit_datas.attribute
              }
            , { form_edit_view = wrap_form_data_view model.frame_edit_datas.battle_text_struct
              , frame_type = model.frame_edit_datas.battle_text_struct
              }
            ]

        tab_items : List (Tab.Item Msg)
        tab_items =
            List.map (do_render_tab model) tab_configs
    in
    Tab.config GotTabMsg
        |> Tab.useHash True
        |> Tab.items tab_items
        |> Tab.view model.active_tab


view : Model -> Html Msg
view model =
    div []
        [ tabs_view model
        ]


form_data_view : FrameEditData obj Msg -> Html Msg
form_data_view frame_edit_data =
-- form_data_view : FrameType -> Html Msg
-- form_data_view frame_type =
    let
        -- frame_edit_data : FrameEditData f msg
        -- frame_edit_data = case frame_type of
        --     WeaponFrameType fed -> fed
        --     ArmorFrameType fed -> fed
        --     ZoneFrameType fed -> fed
        --     WeaponCategoryFrameType fed -> fed
        --     AttributeFrameType fed -> fed
        --     BattleTextStructFrameType fed -> fed

        { frame_data, form_definition, saved_frame_data } =
            frame_edit_data
    in
    Grid.row [ Row.centerMd ]
        [ Grid.col [ Col.sm11, Col.md8 ]
            [ Form.form []
                [ FormData.render_fields form_definition.fields frame_data
                , Button.button
                    [ Button.onClick SubmitFrameEditForm
                    , Button.primary
                    ]
                    [ text "Submit" ]
                ]
            ]
        ]
