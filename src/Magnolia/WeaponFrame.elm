module Magnolia.WeaponFrame exposing (EditFormUpdateType, WeaponFrame, edit_form_definition, update_edit_form_data)

import FormData
    exposing
        ( DataType(..)
        , new_form_field_float
        , new_form_field_int
        , new_form_field_string
        , update_int_field
        )


{-| All these are strings because they get the msg from Html
-}
type EditFormUpdateType
    = Name String
    | FrameId String
    | ChoiceId String
    | PrettyName String
    | Description String
    | FrameImagePath String
    | BonusAttack String
    | BonusPower String
    | BonusEncumbrance String
    | RarityType String
    | CarryWeight String


update_edit_form_data : WeaponFrame -> EditFormUpdateType -> WeaponFrame
update_edit_form_data form_data form_update_type =
    case form_update_type of
        Name new_name ->
            { form_data | weapon_name = new_name }

        ChoiceId new_choice_id ->
            { form_data
                | choice_id = update_int_field form_data.choice_id new_choice_id
            }

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
        name_field : FormData.FormField WeaponFrame msg
        name_field =
            new_form_field_string "weapon_name" .weapon_name (Name >> the_msg)

        frame_id_field : FormData.FormField WeaponFrame msg
        frame_id_field =
            new_form_field_int "frame_id" .frame_id (FrameId >> the_msg)

        choice_id_field : FormData.FormField WeaponFrame msg
        choice_id_field =
            new_form_field_int "choice_id" .choice_id (ChoiceId >> the_msg)
    in
    { fields =
        [ name_field
        , frame_id_field
        , choice_id_field
        , new_form_field_string "description" .description (Description >> the_msg)
        , new_form_field_string "frame_image_path" .frame_image_path (FrameImagePath >> the_msg)
        , new_form_field_int "bonus_attack" .bonus_attack (BonusAttack >> the_msg)
        , new_form_field_int "bonus_power" .bonus_power (BonusPower >> the_msg)
        , new_form_field_int "bonus_encumbrance" .bonus_encumbrance (BonusEncumbrance >> the_msg)
        , new_form_field_int "rarity_type" .rarity_type (RarityType >> the_msg)
        , new_form_field_int "carry_weight" .carry_weight (CarryWeight >> the_msg)
        ]
    }


type BattleRow
    = Melee
    | Ranged
    | Rear

battle_row_from_int : Int -> BattleRow
battle_row_from_int int =
    case int of
        0 -> Melee
        1 -> Ranged
        2 -> Rear
        _ -> Melee

battle_row_to_string :  BattleRow -> String
battle_row_to_string battle_row =
    case battle_row of
        Melee -> "Melee"
        Ranged -> "Ranged"
        Rear -> "Rear"


type WeaponDamageType
    = Unset
    | Piercing
    | Blunt
    | Slashing


weapon_damage_type_from_int : Int -> WeaponDamageType
weapon_damage_type_from_int int =
    case int of
        0 -> Unset
        1 -> Piercing
        2 -> Blunt
        3 -> Slashing
        _ -> Unset

weapon_damage_type_to_string :  WeaponDamageType -> String
weapon_damage_type_to_string damage_type =
    case damage_type of
        Unset -> "Unset"
        Piercing -> "Piercing"
        Blunt -> "Blunt"
        Slashing -> "Slashing"

type alias WeaponFrame =
    { weapon_name : String
    , frame_id : Int
    , choice_id : Int
    , pretty_name : String
    , description : String

    -- , affects_morale', prettyName: "Affects Morale (0, 1)", type: 'hidden'},
    , frame_image_path : String

    -- , battle_row_type', prettyName: "Battle Row", prettifyEnumFn: prettifyBattleRowType, type: 'enum', defaultValue: 0},
    -- , damage_type', prettyName: "Damage Type (PBS)", prettifyEnumFn: prettifyWeaponDamageType, type: 'enum', defaultValue: 0},
    , bonus_attack : Int
    , bonus_power : Int
    , bonus_encumbrance : Int
    , rarity_type : Int
    , carry_weight : Int
    }
