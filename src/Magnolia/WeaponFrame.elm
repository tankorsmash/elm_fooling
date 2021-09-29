module Magnolia.WeaponFrame exposing
    ( BattleRow(..)
    , EditFormUpdateType(..)
    , WeaponDamageType(..)
    , WeaponFrame
    , battle_row_type_from_int
    , download_all_frames
    , edit_form_definition
    , update_edit_form_data
    , weapon_damage_type_from_int
    )

import FormData
    exposing
        ( DataType(..)
        , FieldAlterType
        , InputCallback
        , ignore_alter
        , new_form_field_enum
        , new_form_field_float
        , new_form_field_int
        , new_form_field_string
        , update_enum_field
        , update_int_field
        )
import Http
import Json.Decode as Decode exposing (Decoder, andThen, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Utils exposing (JsonHttpResult, JsonServerResp, clojure_json_server_url, json_server_resp_decoder, root_json_server_url)


{-| All these are strings because they get the msg from Html
-}
type EditFormUpdateType
    = FrameId String
    | PrettyName String
    | Description String
    | FrameImagePath String
    | BattleRowType String
    | WeaponDamageType String
    | BonusAttack String
    | BonusPower String
    | BonusEncumbrance String
    | RarityType String
    | CarryWeight String


update_edit_form_data : WeaponFrame -> EditFormUpdateType -> WeaponFrame
update_edit_form_data form_data form_update_type =
    case form_update_type of
        FrameId new_frame_id ->
            { form_data
                | frame_id = update_int_field form_data.frame_id new_frame_id
            }

        PrettyName new_pretty_name ->
            { form_data | pretty_name = new_pretty_name }

        Description new_description ->
            { form_data | description = new_description }

        FrameImagePath new_frame_image_path ->
            { form_data | frame_image_path = new_frame_image_path }

        BattleRowType new_battle_row_type ->
            { form_data | battle_row_type = update_enum_field form_data.battle_row_type new_battle_row_type battle_row_type_from_int }

        WeaponDamageType new_weapon_damage_type ->
            { form_data | damage_type = update_enum_field form_data.damage_type new_weapon_damage_type weapon_damage_type_from_int }

        BonusAttack new_bonus_attack ->
            { form_data | bonus_attack = update_int_field form_data.bonus_attack new_bonus_attack }

        BonusPower new_bonus_power ->
            { form_data | bonus_power = update_int_field form_data.bonus_power new_bonus_power }

        BonusEncumbrance new_bonus_encumbrance ->
            { form_data | bonus_encumbrance = update_int_field form_data.bonus_encumbrance new_bonus_encumbrance }

        RarityType new_rarity_type ->
            { form_data | rarity_type = update_int_field form_data.rarity_type new_rarity_type }

        CarryWeight new_carry_weight ->
            { form_data | carry_weight = update_int_field form_data.carry_weight new_carry_weight }


edit_form_definition : (EditFormUpdateType -> msg) -> FormData.FormDefinition WeaponFrame msg
edit_form_definition the_msg =
    let
        damage_type_options =
            []

        damage_type_accessor : FormData.EnumAccessor WeaponFrame
        damage_type_accessor =
            .damage_type >> weapon_damage_type_to_string

        damage_type_msg : FieldAlterType -> InputCallback msg
        damage_type_msg =
            ignore_alter <| (WeaponDamageType >> the_msg)

        battle_row_type_accessor : FormData.EnumAccessor WeaponFrame
        battle_row_type_accessor =
            .battle_row_type >> battle_row_to_string

        battle_row_type_msg =
            ignore_alter <| (BattleRowType >> the_msg)
    in
    { fields =
        [ new_form_field_int "frame_id" .frame_id <| ignore_alter <| FrameId >> the_msg
        , new_form_field_string "description" .description <| ignore_alter <| Description >> the_msg
        , new_form_field_string "frame_image_path" .frame_image_path <| ignore_alter <| FrameImagePath >> the_msg
        , new_form_field_enum "battle_row_type" battle_row_type_accessor battle_row_type_msg battle_row_type_values
        , new_form_field_enum "damage_type" damage_type_accessor damage_type_msg weapon_damage_type_values
        , new_form_field_int "bonus_attack" .bonus_attack <| ignore_alter <| BonusAttack >> the_msg
        , new_form_field_int "bonus_power" .bonus_power <| ignore_alter <| BonusPower >> the_msg
        , new_form_field_int "bonus_encumbrance" .bonus_encumbrance <| ignore_alter <| BonusEncumbrance >> the_msg
        , new_form_field_int "rarity_type" .rarity_type <| ignore_alter <| RarityType >> the_msg
        , new_form_field_int "carry_weight" .carry_weight <| ignore_alter <| CarryWeight >> the_msg
        ]
    }


type BattleRow
    = Melee
    | Ranged
    | Rear


battle_row_type_from_int : Int -> BattleRow
battle_row_type_from_int int =
    case int of
        0 ->
            Melee

        1 ->
            Ranged

        2 ->
            Rear

        _ ->
            Melee


battle_row_type_values : List ( String, String )
battle_row_type_values =
    let
        thing =
            Melee

        --hack to bring me back here if BattleRow changes
        ignored _ =
            case thing of
                Melee ->
                    ()

                Ranged ->
                    ()

                Rear ->
                    ()
    in
    [ ( "0", "Melee" )
    , ( "1", "Ranged" )
    , ( "2", "Rear" )
    ]


battle_row_to_string : BattleRow -> String
battle_row_to_string battle_row =
    case battle_row of
        Melee ->
            "Melee"

        Ranged ->
            "Ranged"

        Rear ->
            "Rear"


type WeaponDamageType
    = Unset
    | Piercing
    | Blunt
    | Slashing


weapon_damage_type_from_int : Int -> WeaponDamageType
weapon_damage_type_from_int int =
    case int of
        0 ->
            Unset

        1 ->
            Piercing

        2 ->
            Blunt

        3 ->
            Slashing

        _ ->
            Unset


weapon_damage_type_to_string : WeaponDamageType -> String
weapon_damage_type_to_string damage_type =
    case damage_type of
        Unset ->
            "Unset"

        Piercing ->
            "Piercing"

        Blunt ->
            "Blunt"

        Slashing ->
            "Slashing"


weapon_damage_type_values : List ( String, String )
weapon_damage_type_values =
    let
        thing =
            Piercing

        --hack to bring me back here if BattleRow changes
        ignored _ =
            case thing of
                Unset ->
                    ()

                Piercing ->
                    ()

                Blunt ->
                    ()

                Slashing ->
                    ()
    in
    [ ( "0", "Unset" )
    , ( "1", "Piercing" )
    , ( "2", "Blunt" )
    , ( "2", "Slashing" )
    ]


decode_battle_row : Decoder BattleRow
decode_battle_row =
    -- Json.Decode.succeed BattleRow
    int |> andThen (succeed << battle_row_type_from_int)


decode_weapon_damage_type : Decoder WeaponDamageType
decode_weapon_damage_type =
    -- Json.Decode.succeed WeaponDamageType
    int |> andThen (succeed << weapon_damage_type_from_int)


decode_weapon_frame : Decoder WeaponFrame
decode_weapon_frame =
    succeed WeaponFrame
        |> required "frame_id" int
        |> required "pretty_name" string
        |> required "description" string
        -- |> required- , affects_morale', prettyName: "Affects Morale (0, 1)", type: 'hidden'},
        |> required "frame_image_path" string
        |> required "battle_row_type" decode_battle_row
        |> required "damage_type" decode_weapon_damage_type
        |> required "bonus_attack" int
        |> required "bonus_power" int
        |> required "bonus_encumbrance" int
        |> required "rarity_type" int
        |> required "carry_weight" int


decode_weapon_frames : Decoder (List WeaponFrame)
decode_weapon_frames =
    list decode_weapon_frame


download_all_frames : (JsonHttpResult (List WeaponFrame) -> msg) -> Cmd msg
download_all_frames callback =
    Http.get
        { url = clojure_json_server_url ++ "api/frames/weapon"
        , expect = Http.expectJson callback (json_server_resp_decoder decode_weapon_frames)
        }


type alias WeaponFrame =
    { frame_id : Int
    , pretty_name : String
    , description : String

    -- , affects_morale', prettyName: "Affects Morale (0, 1)", type: 'hidden'},
    , frame_image_path : String
    , battle_row_type : BattleRow
    , damage_type : WeaponDamageType
    , bonus_attack : Int
    , bonus_power : Int
    , bonus_encumbrance : Int
    , rarity_type : Int
    , carry_weight : Int
    }
