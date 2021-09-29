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
import Utils exposing (JsonHttpResult, JsonServerResp, add_class, clojure_json_server_url, root_json_server_url)
import Weather


type GotFrameEditFormUpdateMsg
    = GotEditWeaponFormUpdate Magnolia.WeaponFrame.EditFormUpdateType
    | GotEditZoneFormUpdate Magnolia.ZoneFrame.EditFormUpdateType
    | GotEditWeaponCategoryFormUpdate Magnolia.WeaponCategoryFrame.EditFormUpdateType
    | GotEditAttributeFormUpdate Magnolia.AttributeFrame.EditFormUpdateType
    | GotEditBattleTextStructFormUpdate Magnolia.BattleTextStructFrame.EditFormUpdateType
    | GotEditArmorFormUpdate Magnolia.ArmorFrame.EditFormUpdateType


type AllFramesDownloaded
    = DownloadedAllWeaponFrames (JsonHttpResult (List WeaponFrame))
    | DownloadedAllArmorFrames (JsonHttpResult (List ArmorFrame))
    | DownloadedAllZoneFrames (JsonHttpResult (List ZoneFrame))
    | DownloadedAllWeaponCategoryFrames (JsonHttpResult (List WeaponCategoryFrame))
    | DownloadedAllAttributeFrames (JsonHttpResult (List AttributeFrame))
    | DownloadedAllBattleTextStructFrames (JsonHttpResult (List BattleTextStructFrame))


type TableRowClickedFrameType
    = ClickedTableRowWeaponFrame WeaponFrame
    | ClickedTableRowArmorFrame ArmorFrame
    | ClickedTableRowZoneFrame ZoneFrame
    | ClickedTableRowWeaponCategoryFrame WeaponCategoryFrame
    | ClickedTableRowAttributeFrame AttributeFrame
    | ClickedTableRowBattleTextStructFrame BattleTextStructFrame


type Msg
    = ToggleFrameViewMode
    | SetFrameViewMode FrameViewMode
    | GotFrameEditFormUpdate GotFrameEditFormUpdateMsg
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
    | TableRowClicked FrameData


type OutMsg
    = Noop
    | ToVisualOutput String


type FrameFormDefinition msg
    = WeaponFrameForm (FormData.FormDefinition WeaponFrame msg)
    | ArmorFrameForm (FormData.FormDefinition ArmorFrame msg)
    | ZoneFrameForm (FormData.FormDefinition ZoneFrame msg)
    | WeaponCategoryFrameForm (FormData.FormDefinition WeaponCategoryFrame msg)
    | AttributeFrameForm (FormData.FormDefinition AttributeFrame msg)
    | BattleTextStructFrameForm (FormData.FormDefinition BattleTextStructFrame msg)


type alias FrameEditData msg =
    { form_definition : FrameFormDefinition msg
    , all_frames : List FrameData
    , frame_data : FrameData
    , saved_frame_data : Maybe FrameData
    , table_view_page_info : Table.PageInfo msg
    , frame_type : FrameType
    , frame_type_str : String -- "weapon", "zone", "weapon_category", etc
    , frame_id_getter :
        FrameData
        -> String -- "frame_id", "id", etc. Zones use data_names pretty sure so this'll get hairy
    }


{-| simple enum to determine which is what
-}
type FrameType
    = WeaponFrameType
    | ArmorFrameType
    | ZoneFrameType
    | WeaponCategoryFrameType
    | AttributeFrameType
    | BattleTextStructFrameType


{-| the actual frame data i'd get from jsonm
-}
type FrameData
    = WeaponFrameData WeaponFrame
    | ArmorFrameData ArmorFrame
    | ZoneFrameData ZoneFrame
    | WeaponCategoryFrameData WeaponCategoryFrame
    | AttributeFrameData AttributeFrame
    | BattleTextStructFrameData BattleTextStructFrame


to_string : FrameType -> String
to_string frame_type =
    case frame_type of
        WeaponFrameType ->
            "WeaponFrame"

        ArmorFrameType ->
            "ArmorFrame"

        ZoneFrameType ->
            "ZoneFrame"

        WeaponCategoryFrameType ->
            "WeaponCategoryFrame"

        AttributeFrameType ->
            "AttributeFrame"

        BattleTextStructFrameType ->
            "BattleTextStructFrame"


to_data_name : FrameType -> String
to_data_name frame_type =
    case frame_type of
        WeaponFrameType ->
            "weapon"

        ArmorFrameType ->
            "armor"

        ZoneFrameType ->
            "zone"

        WeaponCategoryFrameType ->
            "weapon_category"

        AttributeFrameType ->
            "attribute"

        BattleTextStructFrameType ->
            "battle_text_struct"


from_data_name : String -> Maybe FrameType
from_data_name frame_type_str =
    case frame_type_str of
        "weapon" ->
            Just WeaponFrameType

        "armor" ->
            Just ArmorFrameType

        "zone" ->
            Just ZoneFrameType

        "weapon_category" ->
            Just WeaponCategoryFrameType

        "attribute" ->
            Just AttributeFrameType

        "battle_text_struct" ->
            Just BattleTextStructFrameType

        _ ->
            Nothing


type alias FrameEditDatas =
    { weapon : FrameEditData Msg
    , armor : FrameEditData Msg
    , zone : FrameEditData Msg
    , weapon_category : FrameEditData Msg
    , attribute : FrameEditData Msg
    , battle_text_struct : FrameEditData Msg
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


frame_id_getter : FrameData -> String
frame_id_getter frame_data =
    case frame_data of
        WeaponFrameData raw_frame_data ->
            String.fromInt raw_frame_data.frame_id

        ArmorFrameData raw_frame_data ->
            String.fromInt raw_frame_data.frame_id

        ZoneFrameData raw_frame_data ->
            raw_frame_data.data_name

        WeaponCategoryFrameData raw_frame_data ->
            String.fromInt raw_frame_data.frame_id

        AttributeFrameData raw_frame_data ->
            String.fromInt raw_frame_data.frame_id

        BattleTextStructFrameData raw_frame_data ->
            String.fromInt raw_frame_data.frame_id


frame_type_from_hash : String -> FrameType
frame_type_from_hash hash =
    if String.startsWith tab_prefix hash then
        let
            suffix =
                String.dropLeft (String.length tab_prefix) hash
        in
        case suffix of
            "weapon_frame" ->
                WeaponFrameType

            "armor_frame" ->
                ArmorFrameType

            "zone_frame" ->
                ZoneFrameType

            "weapon_category_frame" ->
                WeaponCategoryFrameType

            "attribute_frame" ->
                AttributeFrameType

            "battle_text_struct_frame" ->
                BattleTextStructFrameType

            _ ->
                Debug.log ("error: unknown frame type suffix: " ++ suffix) WeaponFrameType

    else
        WeaponFrameType


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

        saved_weapon_frame_data : Maybe FrameData
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

        saved_armor_frame_data : Maybe FrameData
        saved_armor_frame_data =
            Nothing

        zone_frame_data : ZoneFrame
        zone_frame_data =
            { name = "unset in init zone"
            , data_name = "unset_data_name"
            , required_zone_data_name_to_unlock = ""
            , location_data_names_in_the_zone = [ "first", "second" ]
            }

        saved_zone_frame_data : Maybe FrameData
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

        saved_weapon_category_frame_data : Maybe FrameData
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

        saved_attribute_frame_data : Maybe FrameData
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

        saved_battle_text_struct_frame_data : Maybe FrameData
        saved_battle_text_struct_frame_data =
            Nothing

        initial_active_tab_frame_type : FrameType
        initial_active_tab_frame_type =
            frame_type_from_hash hash

        initial_active_tab =
            case hash of
                "" ->
                    Tab.customInitialState <| tab_prefix ++ "weapon_frame"

                _ ->
                    Tab.customInitialState <| hash

        init_model : Model
        init_model =
            { frame_edit_datas =
                { weapon =
                    { form_definition = WeaponFrameForm <| Magnolia.WeaponFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponFormUpdate)
                    , frame_data = WeaponFrameData weapon_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_weapon_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg WeaponFrameType)
                    , frame_type = WeaponFrameType
                    , frame_type_str = to_data_name WeaponFrameType
                    , frame_id_getter = frame_id_getter
                    }
                , armor =
                    { form_definition = ArmorFrameForm <| Magnolia.ArmorFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditArmorFormUpdate)
                    , frame_data = ArmorFrameData armor_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_armor_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg ArmorFrameType)
                    , frame_type = ArmorFrameType
                    , frame_type_str = to_data_name ArmorFrameType
                    , frame_id_getter = frame_id_getter
                    }
                , zone =
                    { form_definition = ZoneFrameForm <| Magnolia.ZoneFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditZoneFormUpdate)
                    , frame_data = ZoneFrameData zone_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_zone_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg ZoneFrameType)
                    , frame_type = ZoneFrameType
                    , frame_type_str = to_data_name ZoneFrameType
                    , frame_id_getter = frame_id_getter
                    }
                , weapon_category =
                    { form_definition = WeaponCategoryFrameForm <| Magnolia.WeaponCategoryFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponCategoryFormUpdate)
                    , frame_data = WeaponCategoryFrameData weapon_category_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_weapon_category_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg WeaponCategoryFrameType)
                    , frame_type = WeaponCategoryFrameType
                    , frame_type_str = to_data_name WeaponCategoryFrameType
                    , frame_id_getter = frame_id_getter
                    }
                , attribute =
                    { form_definition = AttributeFrameForm <| Magnolia.AttributeFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditAttributeFormUpdate)
                    , frame_data = AttributeFrameData attribute_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_attribute_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg AttributeFrameType)
                    , frame_type = AttributeFrameType
                    , frame_type_str = to_data_name AttributeFrameType
                    , frame_id_getter = frame_id_getter
                    }
                , battle_text_struct =
                    { form_definition = BattleTextStructFrameForm <| Magnolia.BattleTextStructFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditBattleTextStructFormUpdate)
                    , frame_data = BattleTextStructFrameData battle_text_struct_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_battle_text_struct_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg BattleTextStructFrameType)
                    , frame_type = BattleTextStructFrameType
                    , frame_type_str = to_data_name BattleTextStructFrameType
                    , frame_id_getter = frame_id_getter
                    }
                }
            , active_tab = initial_active_tab
            , active_tab_frame_type = initial_active_tab_frame_type

            -- , frame_view_mode = List
            , frame_view_mode = Edit
            }

        -- init_cmds = Cmd.none
        init_cmds =
            Cmd.batch
                [-- Task.perform (\_ -> DoDownloadAllFrames initial_active_tab_frame_type) Time.now
                ]
    in
    ( init_model, init_cmds )


update_frame_edit_data :
    FrameEditData msg
    -> UpdateEditFormFunc updateMsg
    -> updateMsg
    -> FrameEditData msg
update_frame_edit_data frame_edit_data update_edit_form_data form_update_type =
    let
        frame_data =
            frame_edit_data.frame_data
    in
    { frame_edit_data | frame_data = update_edit_form_data frame_data form_update_type }


{-| takes a FrameEditDatas and a single FrameEditData and updates the Feds with the single one
-}
type alias FedsUpdater msg =
    FrameEditDatas -> FrameEditData msg -> FrameEditDatas


{-| returns a single Fed from a FrameEditDatas
-}
type alias FedGetter msg =
    FrameEditDatas -> FrameEditData msg


{-| takes a FrameData and a field update type (Name, Description) and returns the updated frame data
-}
type alias UpdateEditFormFunc updateType =
    FrameData -> updateType -> FrameData


update_single_fed_frame_data : FrameEditData msg -> FrameData -> FrameEditData msg
update_single_fed_frame_data fed new_frame_data =
    { fed | frame_data = new_frame_data }


update_feds : FrameEditDatas -> FedsUpdater msg -> FrameEditData msg -> FrameEditDatas
update_feds feds feds_updater new_fed =
    feds_updater feds new_fed


{-| updates the model's FrameEditDatas, using the UpdateEditFormFunc
-}
update_frame_edit_datas :
    Model
    -> FedGetter msg
    -> FedsUpdater msg
    -> UpdateEditFormFunc updateType
    -> updateType
    -> Model
update_frame_edit_datas model fed_getter feds_updater update_edit_form_data form_update_type =
    let
        existing_feds : FrameEditDatas
        existing_feds =
            model.frame_edit_datas

        existing_fed : FrameEditData msg
        existing_fed =
            fed_getter existing_feds

        new_frame_data : FrameData
        new_frame_data =
            update_edit_form_data existing_fed.frame_data form_update_type

        new_fed : FrameEditData msg
        new_fed =
            update_single_fed_frame_data existing_fed new_frame_data
    in
    { model | frame_edit_datas = feds_updater existing_feds new_fed }


update_got_frame_download_all_frames_update : Model -> Msg -> ( Model, Cmd Msg )
update_got_frame_download_all_frames_update model sub_msg =
    ( model, Cmd.none )


update_got_frame_edit_form_update : Model -> GotFrameEditFormUpdateMsg -> ( Model, Cmd Msg )
update_got_frame_edit_form_update model sub_msg =
    case sub_msg of
        GotEditWeaponFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .weapon
                update_fed_weapon
                (\frame_data_ update_type ->
                    case frame_data_ of
                        WeaponFrameData raw_frame_data ->
                            WeaponFrameData <|
                                Magnolia.WeaponFrame.update_edit_form_data
                                    raw_frame_data
                                    update_type

                        _ ->
                            Debug.todo "handle non weapon frame updates"
                )
                form_update_type
            , Cmd.none
            )

        GotEditArmorFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .armor
                update_fed_armor
                (\frame_data_ update_type ->
                    case frame_data_ of
                        ArmorFrameData raw_frame_data ->
                            ArmorFrameData <|
                                Magnolia.ArmorFrame.update_edit_form_data
                                    raw_frame_data
                                    update_type

                        _ ->
                            Debug.todo "handle non armor frame updates"
                )
                form_update_type
            , Cmd.none
            )

        GotEditZoneFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .zone
                update_fed_zone
                (\frame_data_ update_type ->
                    case frame_data_ of
                        ZoneFrameData raw_frame_data ->
                            ZoneFrameData <|
                                Magnolia.ZoneFrame.update_edit_form_data
                                    raw_frame_data
                                    update_type

                        _ ->
                            Debug.todo "handle non zone frame updates"
                )
                form_update_type
            , Cmd.none
            )

        GotEditWeaponCategoryFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .weapon_category
                update_fed_weapon_category
                (\frame_data_ update_type ->
                    case frame_data_ of
                        WeaponCategoryFrameData raw_frame_data ->
                            WeaponCategoryFrameData <|
                                Magnolia.WeaponCategoryFrame.update_edit_form_data
                                    raw_frame_data
                                    update_type

                        _ ->
                            Debug.todo "handle non weapon category frame updates"
                )
                form_update_type
            , Cmd.none
            )

        GotEditAttributeFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .attribute
                update_fed_attribute
                (\frame_data_ update_type ->
                    case frame_data_ of
                        AttributeFrameData raw_frame_data ->
                            AttributeFrameData <|
                                Magnolia.AttributeFrame.update_edit_form_data
                                    raw_frame_data
                                    update_type

                        _ ->
                            Debug.todo "handle non attribute frame updates"
                )
                form_update_type
            , Cmd.none
            )

        GotEditBattleTextStructFormUpdate form_update_type ->
            ( update_frame_edit_datas
                model
                .battle_text_struct
                update_fed_battle_text_struct
                (\frame_data_ update_type ->
                    case frame_data_ of
                        BattleTextStructFrameData raw_frame_data ->
                            BattleTextStructFrameData <|
                                Magnolia.BattleTextStructFrame.update_edit_form_data
                                    raw_frame_data
                                    form_update_type

                        _ ->
                            Debug.todo "handle non battle text struct frame updates"
                )
                form_update_type
            , Cmd.none
            )


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


handle_feds_download :
    FrameEditDatas
    -> FedGetter msg
    -> FedsUpdater msg
    -> Maybe (List FrameData)
    -> FrameEditDatas
handle_feds_download existing_feds fed_getter feds_updater maybe_all_frames =
    let
        existing_fed : FrameEditData msg
        existing_fed =
            fed_getter existing_feds

        all_frames : List FrameData
        all_frames =
            case maybe_all_frames of
                Just new_all_frames ->
                    new_all_frames

                Nothing ->
                    existing_fed.all_frames

        new_page_info =
            Table.initialize_page_info existing_fed.table_view_page_info all_frames

        new_fed : FrameEditData msg
        new_fed =
            { existing_fed
                | all_frames = all_frames
                , table_view_page_info = new_page_info
            }
    in
    feds_updater existing_feds new_fed


update_do_download_all_frames : Model -> FrameType -> ( Model, Cmd Msg )
update_do_download_all_frames model frame_type =
    case frame_type of
        WeaponFrameType ->
            ( model, Magnolia.WeaponFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllWeaponFrames) )

        ArmorFrameType ->
            ( model, Magnolia.ArmorFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllArmorFrames) )

        ZoneFrameType ->
            ( model, Magnolia.ZoneFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllZoneFrames) )

        WeaponCategoryFrameType ->
            ( model, Magnolia.WeaponCategoryFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllWeaponCategoryFrames) )

        AttributeFrameType ->
            ( model, Magnolia.AttributeFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllAttributeFrames) )

        BattleTextStructFrameType ->
            ( model, Magnolia.BattleTextStructFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllBattleTextStructFrames) )



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
                    handle_feds_download
                        feds
                        .weapon
                        update_fed_weapon
                    <|
                        case unpack_response response of
                            Just all_weapon_frames ->
                                Just <| List.map WeaponFrameData all_weapon_frames.data

                            Nothing ->
                                Nothing

                DownloadedAllArmorFrames response ->
                    handle_feds_download
                        feds
                        .armor
                        update_fed_armor
                    <|
                        case unpack_response response of
                            Just all_armor_frames ->
                                Just <| List.map ArmorFrameData all_armor_frames.data

                            Nothing ->
                                Nothing

                DownloadedAllZoneFrames response ->
                    handle_feds_download
                        feds
                        .zone
                        update_fed_zone
                    <|
                        case unpack_response response of
                            Just all_zone_frames ->
                                Just <| List.map ZoneFrameData all_zone_frames.data

                            Nothing ->
                                Nothing

                DownloadedAllWeaponCategoryFrames response ->
                    handle_feds_download
                        feds
                        .weapon_category
                        update_fed_weapon_category
                    <|
                        case unpack_response response of
                            Just all_weapon_category_frames ->
                                Just <| List.map WeaponCategoryFrameData all_weapon_category_frames.data

                            Nothing ->
                                Nothing

                DownloadedAllAttributeFrames response ->
                    handle_feds_download
                        feds
                        .attribute
                        update_fed_attribute
                    <|
                        case unpack_response response of
                            Just all_attribute_frames ->
                                Just <| List.map AttributeFrameData all_attribute_frames.data

                            Nothing ->
                                Nothing

                DownloadedAllBattleTextStructFrames response ->
                    handle_feds_download
                        feds
                        .battle_text_struct
                        update_fed_battle_text_struct
                    <|
                        case unpack_response response of
                            Just all_battle_text_struct_frames ->
                                Just <| List.map BattleTextStructFrameData all_battle_text_struct_frames.data

                            Nothing ->
                                Nothing
    in
    ( { model | frame_edit_datas = new_feds }, cmd )


update_fed_weapon : FrameEditDatas -> FrameEditData Msg -> FrameEditDatas
update_fed_weapon feds new_fed =
    { feds | weapon = new_fed }


update_fed_armor : FrameEditDatas -> FrameEditData Msg -> FrameEditDatas
update_fed_armor feds new_fed =
    { feds | armor = new_fed }


update_fed_zone : FrameEditDatas -> FrameEditData Msg -> FrameEditDatas
update_fed_zone feds new_fed =
    { feds | zone = new_fed }


update_fed_weapon_category : FrameEditDatas -> FrameEditData Msg -> FrameEditDatas
update_fed_weapon_category feds new_fed =
    { feds | weapon_category = new_fed }


update_fed_attribute : FrameEditDatas -> FrameEditData Msg -> FrameEditDatas
update_fed_attribute feds new_fed =
    { feds | attribute = new_fed }


update_fed_battle_text_struct : FrameEditDatas -> FrameEditData Msg -> FrameEditDatas
update_fed_battle_text_struct feds new_fed =
    { feds | battle_text_struct = new_fed }


update_table_row_clicked_frame_type :
    Model
    -> FrameData
    -> ( Model, Cmd Msg )
update_table_row_clicked_frame_type model sub_msg =
    let
        feds =
            model.frame_edit_datas

        new_feds =
            case sub_msg of
                WeaponFrameData raw_frame_data ->
                    update_fed_weapon feds <|
                        (update_single_fed_frame_data feds.weapon <|
                            WeaponFrameData raw_frame_data
                        )

                ArmorFrameData raw_frame_data ->
                    update_fed_armor feds <|
                        (update_single_fed_frame_data feds.armor <|
                            ArmorFrameData raw_frame_data
                        )

                ZoneFrameData raw_frame_data ->
                    update_fed_zone feds <|
                        update_single_fed_frame_data feds.zone <|
                            ZoneFrameData raw_frame_data

                WeaponCategoryFrameData raw_frame_data ->
                    update_fed_weapon_category feds <|
                        update_single_fed_frame_data feds.weapon_category <|
                            WeaponCategoryFrameData raw_frame_data

                AttributeFrameData raw_frame_data ->
                    update_fed_attribute feds <|
                        update_single_fed_frame_data feds.attribute <|
                            AttributeFrameData raw_frame_data

                BattleTextStructFrameData raw_frame_data ->
                    update_fed_battle_text_struct feds <|
                        update_single_fed_frame_data feds.battle_text_struct <|
                            BattleTextStructFrameData raw_frame_data

        -- toggle the ui so we are looking at the detail mode
        ( new_model, new_cmd, _ ) =
            update { model | frame_edit_datas = new_feds } ToggleFrameViewMode
    in
    ( new_model, new_cmd )


{-| uses frame\_type to figure out which FED's frame\_data to look up
-}
get_frame_id_from_feds_frame_data : FrameEditDatas -> FrameType -> String
get_frame_id_from_feds_frame_data feds frame_type =
    case frame_type of
        WeaponFrameType ->
            feds.weapon.frame_id_getter feds.weapon.frame_data

        ArmorFrameType ->
            feds.armor.frame_id_getter feds.armor.frame_data

        ZoneFrameType ->
            feds.zone.frame_id_getter feds.zone.frame_data

        WeaponCategoryFrameType ->
            feds.weapon_category.frame_id_getter feds.weapon_category.frame_data

        AttributeFrameType ->
            feds.attribute.frame_id_getter feds.attribute.frame_data

        BattleTextStructFrameType ->
            feds.battle_text_struct.frame_id_getter feds.battle_text_struct.frame_data


{-| uses frame\_type to figure out which FED's frame\_data exists in its all\_frames
-}
frame_matches_from_feds_frame_data : FrameEditDatas -> FrameType -> Bool
frame_matches_from_feds_frame_data feds frame_type =
    case frame_type of
        WeaponFrameType ->
            frame_matches feds.weapon.all_frames feds.weapon.frame_data

        ArmorFrameType ->
            frame_matches feds.armor.all_frames feds.armor.frame_data

        ZoneFrameType ->
            frame_matches feds.zone.all_frames feds.zone.frame_data

        WeaponCategoryFrameType ->
            frame_matches feds.weapon_category.all_frames feds.weapon_category.frame_data

        AttributeFrameType ->
            frame_matches feds.attribute.all_frames feds.attribute.frame_data

        BattleTextStructFrameType ->
            frame_matches feds.battle_text_struct.all_frames feds.battle_text_struct.frame_data


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

        GotFrameEditFormUpdate sub_msg ->
            let
                ( model_, cmd ) =
                    update_got_frame_edit_form_update model sub_msg
            in
            ( model_, cmd, Noop )

        SubmitFrameEditForm ->
            -- TODO: make request to clojure server to update frame
            let
                active_frame_type =
                    model.active_tab_frame_type

                whole_frame_data_matches =
                    frame_matches_from_feds_frame_data model.frame_edit_datas active_frame_type

                url_root =
                    case whole_frame_data_matches of
                        True ->
                            "api/frames/" ++ to_data_name active_frame_type

                        False ->
                            "api/create_frame/" ++ to_data_name active_frame_type

                url_suffix =
                    url_root
                        ++ "/"
                        ++ get_frame_id_from_feds_frame_data
                            model.frame_edit_datas
                            active_frame_type

                cmd =
                    Debug.log "submitting"
                        Http.get
                        { url = clojure_json_server_url ++ url_suffix
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
                    frame_type_from_hash hash
            in
            ( { model | active_tab_frame_type = new_frame_type }, Task.perform (\_ -> DoDownloadAllFrames new_frame_type) Time.now, Noop )

        GotPageMsg frame_type page_msg ->
            let
                old_page_info =
                    get_page_info model.frame_edit_datas frame_type

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


get_page_info : FrameEditDatas -> FrameType -> Table.PageInfo Msg
get_page_info feds frame_type =
    case frame_type of
        WeaponFrameType ->
            feds.weapon.table_view_page_info

        ArmorFrameType ->
            feds.armor.table_view_page_info

        ZoneFrameType ->
            feds.zone.table_view_page_info

        WeaponCategoryFrameType ->
            feds.weapon_category.table_view_page_info

        AttributeFrameType ->
            feds.attribute.table_view_page_info

        BattleTextStructFrameType ->
            feds.battle_text_struct.table_view_page_info


update_only_page_info : FrameEditData Msg -> Table.PageInfo Msg -> FrameEditData Msg
update_only_page_info old_fed new_page_info =
    { old_fed | table_view_page_info = new_page_info }


set_page_info : FrameEditDatas -> FrameType -> Table.PageInfo Msg -> FrameEditDatas
set_page_info feds frame_type new_page_info =
    case frame_type of
        WeaponFrameType ->
            update_fed_weapon feds <| update_only_page_info feds.weapon new_page_info

        ArmorFrameType ->
            update_fed_armor feds <| update_only_page_info feds.armor new_page_info

        ZoneFrameType ->
            update_fed_zone feds <| update_only_page_info feds.zone new_page_info

        WeaponCategoryFrameType ->
            update_fed_weapon_category feds <| update_only_page_info feds.weapon_category new_page_info

        AttributeFrameType ->
            update_fed_attribute feds <| update_only_page_info feds.attribute new_page_info

        BattleTextStructFrameType ->
            update_fed_battle_text_struct feds <| update_only_page_info feds.battle_text_struct new_page_info


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


build_table_definition :
    FrameType
    -> List (FormData.FormField fd Msg)
    -> (fd -> Msg)
    -> TableDefinition fd Msg
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
    -> List (Maybe fd)
    -> FormData.FormDefinition fd Msg
    -> (fd -> FrameData)
    -> Tab.Item Msg
render_tab_item model config maybe_row_data form_definition partial_frame_data =
    let
        frame_type : FrameType
        frame_type =
            config.frame_type

        form_fields : List (FormData.FormField fd Msg)
        form_fields =
            form_definition.fields

        table_definition : TableDefinition fd Msg
        table_definition =
            build_table_definition frame_type form_fields (TableRowClicked << partial_frame_data)

        row_data : List fd
        row_data =
            List.filterMap identity maybe_row_data

        page_info : Table.PageInfo Msg
        page_info =
            Table.new_page_info <| GotPageMsg frame_type

        rendered_tab_content : Html Msg
        rendered_tab_content =
            case model.frame_view_mode of
                Edit ->
                    config.form_edit_view

                List ->
                    Table.view
                        table_definition
                        row_data
                        -- frame_edit_data.table_view_page_info
                        page_info

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


get_all_weapon_frames : List FrameData -> List (Maybe WeaponFrame)
get_all_weapon_frames frame_data =
    List.map
        (\frame_data_ ->
            case frame_data_ of
                WeaponFrameData data ->
                    Just data

                _ ->
                    Nothing
        )
        frame_data


get_all_armor_frames : List FrameData -> List (Maybe ArmorFrame)
get_all_armor_frames frame_data =
    List.map
        (\frame_data_ ->
            case frame_data_ of
                ArmorFrameData data ->
                    Just data

                _ ->
                    Nothing
        )
        frame_data


get_all_zone_frames : List FrameData -> List (Maybe ZoneFrame)
get_all_zone_frames frame_data =
    List.map
        (\frame_data_ ->
            case frame_data_ of
                ZoneFrameData data ->
                    Just data

                _ ->
                    Nothing
        )
        frame_data


get_all_weapon_category_frames : List FrameData -> List (Maybe WeaponCategoryFrame)
get_all_weapon_category_frames frame_data =
    List.map
        (\frame_data_ ->
            case frame_data_ of
                WeaponCategoryFrameData data ->
                    Just data

                _ ->
                    Nothing
        )
        frame_data


get_all_attribute_frames : List FrameData -> List (Maybe AttributeFrame)
get_all_attribute_frames frame_data =
    List.map
        (\frame_data_ ->
            case frame_data_ of
                AttributeFrameData data ->
                    Just data

                _ ->
                    Nothing
        )
        frame_data


get_all_battle_text_struct_frames : List FrameData -> List (Maybe BattleTextStructFrame)
get_all_battle_text_struct_frames frame_data =
    List.map
        (\frame_data_ ->
            case frame_data_ of
                BattleTextStructFrameData data ->
                    Just data

                _ ->
                    Nothing
        )
        frame_data


do_render_tab : Model -> TabItemConfig -> Tab.Item Msg
do_render_tab model config =
    case config.frame_type of
        WeaponFrameType ->
            render_tab_item model
                config
                (get_all_weapon_frames model.frame_edit_datas.weapon.all_frames)
                (Magnolia.WeaponFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponFormUpdate))
                WeaponFrameData

        ArmorFrameType ->
            render_tab_item model
                config
                (get_all_armor_frames model.frame_edit_datas.armor.all_frames)
                (Magnolia.ArmorFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditArmorFormUpdate))
                ArmorFrameData

        ZoneFrameType ->
            render_tab_item model
                config
                (get_all_zone_frames model.frame_edit_datas.zone.all_frames)
                (Magnolia.ZoneFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditZoneFormUpdate))
                ZoneFrameData

        WeaponCategoryFrameType ->
            render_tab_item model
                config
                (get_all_weapon_category_frames model.frame_edit_datas.weapon_category.all_frames)
                (Magnolia.WeaponCategoryFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponCategoryFormUpdate))
                WeaponCategoryFrameData

        AttributeFrameType ->
            render_tab_item model
                config
                (get_all_attribute_frames model.frame_edit_datas.attribute.all_frames)
                (Magnolia.AttributeFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditAttributeFormUpdate))
                AttributeFrameData

        BattleTextStructFrameType ->
            render_tab_item model
                config
                (get_all_battle_text_struct_frames model.frame_edit_datas.battle_text_struct.all_frames)
                (Magnolia.BattleTextStructFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditBattleTextStructFormUpdate))
                BattleTextStructFrameData


tabs_view : Model -> Html Msg
tabs_view model =
    let
        tab_configs : List TabItemConfig
        tab_configs =
            [ { form_edit_view = form_data_view model.frame_edit_datas.weapon
              , frame_type = WeaponFrameType
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.armor
              , frame_type = ArmorFrameType
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.zone
              , frame_type = ZoneFrameType
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.weapon_category
              , frame_type = WeaponCategoryFrameType
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.attribute
              , frame_type = AttributeFrameType
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.battle_text_struct
              , frame_type = BattleTextStructFrameType
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


frame_matches : List f -> f -> Bool
frame_matches all_frames frame =
    List.any (\f -> frame == f) all_frames


form_data_view : FrameEditData Msg -> Html Msg
form_data_view frame_edit_data =
    case frame_edit_data.frame_type of
        WeaponFrameType ->
            let
                maybe_frame_data : Maybe WeaponFrame
                maybe_frame_data =
                    case frame_edit_data.frame_data of
                        WeaponFrameData raw_frame_data_ ->
                            Just raw_frame_data_

                        _ ->
                            Nothing

                maybe_form_definition : Maybe (FormData.FormDefinition WeaponFrame Msg)
                maybe_form_definition =
                    case frame_edit_data.form_definition of
                        WeaponFrameForm form_def ->
                            Just form_def

                        _ ->
                            Nothing

                maybe_all_weapon_frames : List (Maybe WeaponFrame)
                maybe_all_weapon_frames =
                    get_all_weapon_frames frame_edit_data.all_frames
            in
            inner_form_data_view maybe_frame_data maybe_form_definition maybe_all_weapon_frames

        ArmorFrameType ->
            let
                maybe_frame_data : Maybe ArmorFrame
                maybe_frame_data =
                    case frame_edit_data.frame_data of
                        ArmorFrameData raw_frame_data_ ->
                            Just raw_frame_data_

                        _ ->
                            Nothing

                maybe_form_definition : Maybe (FormData.FormDefinition ArmorFrame Msg)
                maybe_form_definition =
                    case frame_edit_data.form_definition of
                        ArmorFrameForm form_def ->
                            Just form_def

                        _ ->
                            Nothing

                maybe_all_armor_frames : List (Maybe ArmorFrame)
                maybe_all_armor_frames =
                    get_all_armor_frames frame_edit_data.all_frames
            in
            inner_form_data_view maybe_frame_data maybe_form_definition maybe_all_armor_frames

        ZoneFrameType ->
            let
                maybe_frame_data : Maybe ZoneFrame
                maybe_frame_data =
                    case frame_edit_data.frame_data of
                        ZoneFrameData raw_frame_data_ ->
                            Just raw_frame_data_

                        _ ->
                            Nothing

                maybe_form_definition : Maybe (FormData.FormDefinition ZoneFrame Msg)
                maybe_form_definition =
                    case frame_edit_data.form_definition of
                        ZoneFrameForm form_def ->
                            Just form_def

                        _ ->
                            Nothing

                maybe_all_zone_frames : List (Maybe ZoneFrame)
                maybe_all_zone_frames =
                    get_all_zone_frames frame_edit_data.all_frames
            in
            inner_form_data_view maybe_frame_data maybe_form_definition maybe_all_zone_frames

        WeaponCategoryFrameType ->
            let
                maybe_frame_data : Maybe WeaponCategoryFrame
                maybe_frame_data =
                    case frame_edit_data.frame_data of
                        WeaponCategoryFrameData raw_frame_data_ ->
                            Just raw_frame_data_

                        _ ->
                            Nothing

                maybe_form_definition : Maybe (FormData.FormDefinition WeaponCategoryFrame Msg)
                maybe_form_definition =
                    case frame_edit_data.form_definition of
                        WeaponCategoryFrameForm form_def ->
                            Just form_def

                        _ ->
                            Nothing

                maybe_all_weapon_category_frames : List (Maybe WeaponCategoryFrame)
                maybe_all_weapon_category_frames =
                    get_all_weapon_category_frames frame_edit_data.all_frames
            in
            inner_form_data_view maybe_frame_data maybe_form_definition maybe_all_weapon_category_frames

        AttributeFrameType ->
            let
                maybe_frame_data : Maybe AttributeFrame
                maybe_frame_data =
                    case frame_edit_data.frame_data of
                        AttributeFrameData raw_frame_data_ ->
                            Just raw_frame_data_

                        _ ->
                            Nothing

                maybe_form_definition : Maybe (FormData.FormDefinition AttributeFrame Msg)
                maybe_form_definition =
                    case frame_edit_data.form_definition of
                        AttributeFrameForm form_def ->
                            Just form_def

                        _ ->
                            Nothing

                maybe_all_attribute_frames : List (Maybe AttributeFrame)
                maybe_all_attribute_frames =
                    get_all_attribute_frames frame_edit_data.all_frames
            in
            inner_form_data_view maybe_frame_data maybe_form_definition maybe_all_attribute_frames

        BattleTextStructFrameType ->
            let
                maybe_frame_data : Maybe BattleTextStructFrame
                maybe_frame_data =
                    case frame_edit_data.frame_data of
                        BattleTextStructFrameData raw_frame_data_ ->
                            Just raw_frame_data_

                        _ ->
                            Nothing

                maybe_form_definition : Maybe (FormData.FormDefinition BattleTextStructFrame Msg)
                maybe_form_definition =
                    case frame_edit_data.form_definition of
                        BattleTextStructFrameForm form_def ->
                            Just form_def

                        _ ->
                            Nothing

                maybe_all_battle_text_struct_frames : List (Maybe BattleTextStructFrame)
                maybe_all_battle_text_struct_frames =
                    get_all_battle_text_struct_frames frame_edit_data.all_frames
            in
            inner_form_data_view maybe_frame_data maybe_form_definition maybe_all_battle_text_struct_frames


inner_form_data_view : Maybe fd -> Maybe (FormData.FormDefinition fd Msg) -> List (Maybe fd) -> Html Msg
inner_form_data_view maybe_frame_data maybe_form_definition all_frames =
    let
        frame_data_exists_in_all_frames : Bool
        frame_data_exists_in_all_frames =
            case maybe_frame_data of
                Just frame_data ->
                    frame_matches all_frames maybe_frame_data

                Nothing ->
                    False

        fields : List (FormData.FormField fd Msg)
        fields =
            case maybe_form_definition of
                Just form_definition_ ->
                    form_definition_.fields

                Nothing ->
                    []

        rendered_fields =
            case maybe_frame_data of
                Just raw_frame_data_ ->
                    FormData.render_fields fields raw_frame_data_

                Nothing ->
                    span [] []
    in
    Grid.row [ Row.centerMd ]
        [ Grid.col [ Col.sm11, Col.md8 ]
            [ Form.form []
                [ rendered_fields
                , Button.button
                    [ Button.onClick SubmitFrameEditForm
                    , Button.primary
                    ]
                    [ text <|
                        case frame_data_exists_in_all_frames of
                            True ->
                                "Update"

                            False ->
                                "Create (todo!)"
                    ]
                ]
            ]
        ]
