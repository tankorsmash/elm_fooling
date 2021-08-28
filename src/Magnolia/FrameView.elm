module Magnolia.FrameView exposing (Model, Msg(..), init, update, view)

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
import Utils exposing (add_class, root_json_server_url)
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


type Msg
    = ToggleFrameViewMode
    | GotFrameEditFormUpdate GotFrameEditFormUpdateMsg
    | DoDownloadAllFrames FrameType
      -- | DoDownloadWeaponFrames
    | GotDownloadedAllFrames AllFramesDownloaded
      -- | GotDownloadedWeaponFrames (Result Http.Error (List WeaponFrame))
    | GotPageMsg FrameType PageInfoMsg
    | GotTabMsg Tab.State
    | ClickChangeTab FrameType
    | HashUpdated String --hash


type alias FrameEditData f msg =
    { form_definition : FormData.FormDefinition f msg
    , all_frames : List f
    , frame_data : f
    , saved_frame_data : Maybe f
    , table_view_page_info : Table.PageInfo msg
    , frame_type : FrameType
    }


type FrameType
    = WeaponFrame
    | ArmorFrame
    | ZoneFrame
    | WeaponCategoryFrame
    | AttributeFrame
    | BattleTextStructFrame


to_string : FrameType -> String
to_string frame_type =
    case frame_type of
        WeaponFrame ->
            "WeaponFrame"

        ArmorFrame ->
            "ArmorFrame"

        ZoneFrame ->
            "ZoneFrame"

        WeaponCategoryFrame ->
            "WeaponCategoryFrame"

        AttributeFrame ->
            "AttributeFrame"

        BattleTextStructFrame ->
            "BattleTextStructFrame"


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
    , active_tab_frame_type : FrameType
    }


tab_prefix : String
tab_prefix =
    "frame_view_tab__"


frame_type_from_hash : String -> FrameType
frame_type_from_hash hash =
    if String.startsWith tab_prefix hash then
        let
            suffix =
                String.dropLeft (String.length tab_prefix) hash
        in
        case suffix of
            "weapon_frame" ->
                WeaponFrame

            "armor_frame" ->
                ArmorFrame

            "zone_frame" ->
                ZoneFrame

            "weapon_category_frame" ->
                WeaponCategoryFrame

            "attribute_frame" ->
                AttributeFrame

            "battle_text_struct_frame" ->
                BattleTextStructFrame

            _ ->
                Debug.log ("error: unknown frame type suffix: " ++ suffix) WeaponFrame

    else
        WeaponFrame


init : String -> ( Model, Cmd Msg )
init hash =
    let
        weapon_frame_data : WeaponFrame
        weapon_frame_data =
            { frame_id = 123
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
                    { form_definition = Magnolia.WeaponFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponFormUpdate)
                    , frame_data = weapon_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_weapon_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg WeaponFrame)
                    , frame_type = WeaponFrame
                    }
                , armor =
                    { form_definition = Magnolia.ArmorFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditArmorFormUpdate)
                    , frame_data = armor_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_armor_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg ArmorFrame)
                    , frame_type = ArmorFrame
                    }
                , zone =
                    { form_definition = Magnolia.ZoneFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditZoneFormUpdate)
                    , frame_data = zone_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_zone_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg ZoneFrame)
                    , frame_type = ZoneFrame
                    }
                , weapon_category =
                    { form_definition = Magnolia.WeaponCategoryFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponCategoryFormUpdate)
                    , frame_data = weapon_category_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_weapon_category_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg WeaponCategoryFrame)
                    , frame_type = WeaponCategoryFrame
                    }
                , attribute =
                    { form_definition = Magnolia.AttributeFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditAttributeFormUpdate)
                    , frame_data = attribute_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_attribute_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg AttributeFrame)
                    , frame_type = AttributeFrame
                    }
                , battle_text_struct =
                    { form_definition = Magnolia.BattleTextStructFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditBattleTextStructFormUpdate)
                    , frame_data = battle_text_struct_frame_data
                    , all_frames = []
                    , saved_frame_data = saved_battle_text_struct_frame_data
                    , table_view_page_info = Table.new_page_info (GotPageMsg BattleTextStructFrame)
                    , frame_type = BattleTextStructFrame
                    }
                }
            , active_tab = initial_active_tab
            , active_tab_frame_type = initial_active_tab_frame_type
            , frame_view_mode = List
            }

        -- init_cmds = Cmd.none
        init_cmds =
            Cmd.batch [ Task.perform (\_ -> DoDownloadAllFrames initial_active_tab_frame_type) Time.now ]
    in
    ( init_model, init_cmds )


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
        existing_feds : FrameEditDatas
        existing_feds =
            model.frame_edit_datas

        existing_fed : FrameEditData frameData msg
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
    -> FedGetter frameData msg
    -> FedsUpdater frameData msg
    -> Maybe (List frameData)
    -> FrameEditDatas
handle_feds_download existing_feds fed_getter feds_updater maybe_all_frames =
    let
        existing_fed : FrameEditData frameData msg
        existing_fed =
            fed_getter existing_feds

        all_frames : List frameData
        all_frames =
            case maybe_all_frames of
                Just new_all_frames ->
                    new_all_frames

                Nothing ->
                    existing_fed.all_frames

        new_page_info =
            Table.initialize_page_info existing_fed.table_view_page_info all_frames

        new_fed : FrameEditData frameData msg
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
        WeaponFrame ->
            ( model, Magnolia.WeaponFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllWeaponFrames) )

        ArmorFrame ->
            ( model, Magnolia.ArmorFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllArmorFrames) )

        ZoneFrame ->
            ( model, Magnolia.ZoneFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllZoneFrames) )

        WeaponCategoryFrame ->
            ( model, Magnolia.WeaponCategoryFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllWeaponCategoryFrames) )

        AttributeFrame ->
            ( model, Magnolia.AttributeFrame.download_all_frames (GotDownloadedAllFrames << DownloadedAllAttributeFrames) )

        BattleTextStructFrame ->
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
                        (\feds_ new_fed -> { feds_ | weapon = new_fed })
                        (unpack_response response)

                DownloadedAllArmorFrames response ->
                    handle_feds_download
                        feds
                        .armor
                        (\feds_ new_fed -> { feds_ | armor = new_fed })
                        (unpack_response response)

                DownloadedAllZoneFrames response ->
                    handle_feds_download
                        feds
                        .zone
                        (\feds_ new_fed -> { feds_ | zone = new_fed })
                        (unpack_response response)

                DownloadedAllWeaponCategoryFrames response ->
                    handle_feds_download
                        feds
                        .weapon_category
                        (\feds_ new_fed -> { feds_ | weapon_category = new_fed })
                        (unpack_response response)

                DownloadedAllAttributeFrames response ->
                    handle_feds_download
                        feds
                        .attribute
                        (\feds_ new_fed -> { feds_ | attribute = new_fed })
                        (unpack_response response)

                DownloadedAllBattleTextStructFrames response ->
                    handle_feds_download
                        feds
                        .battle_text_struct
                        (\feds_ new_fed -> { feds_ | battle_text_struct = new_fed })
                        (unpack_response response)
    in
    ( { model | frame_edit_datas = new_feds }, cmd )


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

        -- DoDownloadWeaponFrames ->
        DoDownloadAllFrames frame_type ->
            update_do_download_all_frames model frame_type

        -- GotDownloadedWeaponFrames response ->
        GotDownloadedAllFrames all_frames_downloaded ->
            let
                ( new_model, new_cmd ) =
                    update_got_downloaded_all_frames model all_frames_downloaded
            in
            ( new_model, new_cmd )

        GotTabMsg new_state ->
            let
                _ =
                    Debug.log "changing tabs" ""

                -- _ = case new_state of
                --     Just _ -> Debug.log "" ""
            in
            ( { model | active_tab = new_state }, Cmd.none )

        ClickChangeTab frame_type ->
            ( model, Cmd.none )

        HashUpdated hash ->
            let new_frame_type = frame_type_from_hash hash
            in
            ( {model | active_tab_frame_type = new_frame_type} , Task.perform (\_ -> DoDownloadAllFrames new_frame_type) Time.now)

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
    { form_edit_view : Html Msg, frame_type : FrameType }


build_table_definition : FrameType -> List (FormData.FormField fd msg) -> TableDefinition fd
build_table_definition frame_type form_fields =
    { title = Just <| to_string frame_type, columns = List.indexedMap form_field_to_column form_fields }


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
    -> Tab.Item Msg
render_tab_item model config frame_edit_data form_definition =
    let
        frame_type : FrameType
        frame_type =
            config.frame_type

        form_fields : List (FormData.FormField fd Msg)
        form_fields =
            form_definition.fields

        table_definition : TableDefinition fd
        table_definition =
            build_table_definition frame_type form_fields

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
                [ button_primary ToggleFrameViewMode "Toggle View"
                , rendered_tab_content
                ]
        }


do_render_tab : Model -> TabItemConfig -> Tab.Item Msg
do_render_tab model config =
    case config.frame_type of
        WeaponFrame ->
            render_tab_item model
                config
                model.frame_edit_datas.weapon
                (Magnolia.WeaponFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponFormUpdate))

        ArmorFrame ->
            render_tab_item model
                config
                model.frame_edit_datas.armor
                (Magnolia.ArmorFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditArmorFormUpdate))

        ZoneFrame ->
            render_tab_item model
                config
                model.frame_edit_datas.zone
                (Magnolia.ZoneFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditZoneFormUpdate))

        WeaponCategoryFrame ->
            render_tab_item model
                config
                model.frame_edit_datas.weapon_category
                (Magnolia.WeaponCategoryFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditWeaponCategoryFormUpdate))

        AttributeFrame ->
            render_tab_item model
                config
                model.frame_edit_datas.attribute
                (Magnolia.AttributeFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditAttributeFormUpdate))

        BattleTextStructFrame ->
            render_tab_item model
                config
                model.frame_edit_datas.battle_text_struct
                (Magnolia.BattleTextStructFrame.edit_form_definition (GotFrameEditFormUpdate << GotEditBattleTextStructFormUpdate))


tabs_view : Model -> Html Msg
tabs_view model =
    let
        tab_configs : List TabItemConfig
        tab_configs =
            [ { form_edit_view = form_data_view model.frame_edit_datas.weapon
              , frame_type = WeaponFrame
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.armor
              , frame_type = ArmorFrame
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.zone
              , frame_type = ZoneFrame
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.weapon_category
              , frame_type = WeaponCategoryFrame
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.attribute
              , frame_type = AttributeFrame
              }
            , { form_edit_view = form_data_view model.frame_edit_datas.battle_text_struct
              , frame_type = BattleTextStructFrame
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
