module Magnolia.ArmorFrame exposing
    ( ArmorFrame
    , EditFormUpdateType
    , edit_form_definition
    , update_edit_form_data
    )

import FormData
    exposing
        ( DataType(..)
        , ignore_alter
        , new_form_field_enum
        , new_form_field_float
        , new_form_field_int
        , new_form_field_string
        , update_enum_field
        , update_int_field
        )


{-| Need to be strings because it comes from html forms
-}
type EditFormUpdateType
    = FrameId String
    | PrettyName String
    | BonusDefense String
    | BonusProtection String
    | BonusProtectionPiercing String
    | BonusProtectionBlunt String
    | BonusProtectionSlashing String
    | BonusEncumbrance String
    | RarityType String
    | CarryWeight String


update_edit_form_data : ArmorFrame -> EditFormUpdateType -> ArmorFrame
update_edit_form_data form_data form_update_type =
    case form_update_type of
        FrameId new_frame_id ->
            { form_data | frame_id = update_int_field form_data.frame_id new_frame_id }

        PrettyName new_pretty_name ->
            { form_data | pretty_name = new_pretty_name }

        BonusDefense new_bonus_defense ->
            { form_data | bonus_defense = update_int_field form_data.bonus_defense new_bonus_defense }

        BonusProtection new_bonus_protection ->
            { form_data | bonus_protection = update_int_field form_data.bonus_protection new_bonus_protection }

        BonusProtectionPiercing new_bonus_protection_piercing ->
            { form_data | bonus_protection_piercing = update_int_field form_data.bonus_protection_piercing new_bonus_protection_piercing }

        BonusProtectionBlunt new_bonus_protection_blunt ->
            { form_data | bonus_protection_blunt = update_int_field form_data.bonus_protection_blunt new_bonus_protection_blunt }

        BonusProtectionSlashing new_bonus_protection_slashing ->
            { form_data | bonus_protection_slashing = update_int_field form_data.bonus_protection_slashing new_bonus_protection_slashing }

        BonusEncumbrance new_bonus_encumbrance ->
            { form_data | bonus_encumbrance = update_int_field form_data.bonus_encumbrance new_bonus_encumbrance }

        RarityType new_rarity_type ->
            { form_data | rarity_type = update_int_field form_data.rarity_type new_rarity_type }

        CarryWeight new_carry_weight ->
            { form_data | carry_weight = update_int_field form_data.carry_weight new_carry_weight }


edit_form_definition : (EditFormUpdateType -> msg) -> FormData.FormDefinition ArmorFrame msg
edit_form_definition the_msg =
    let
        damage_type_options =
            []
    in
    { fields =
        [ new_form_field_string "pretty_name" .pretty_name <| ignore_alter <| PrettyName >> the_msg
        , new_form_field_int "frame_id" .frame_id <| ignore_alter <| FrameId >> the_msg
        , new_form_field_int "bonus_defense" .bonus_defense <| ignore_alter <| BonusDefense >> the_msg
        , new_form_field_int "bonus_protection" .bonus_protection <| ignore_alter <| BonusProtection >> the_msg
        , new_form_field_int "bonus_protection_piercing" .bonus_protection_piercing <| ignore_alter <| BonusProtectionPiercing >> the_msg
        , new_form_field_int "bonus_protection_blunt" .bonus_protection_blunt <| ignore_alter <| BonusProtectionBlunt >> the_msg
        , new_form_field_int "bonus_protection_slashing" .bonus_protection_slashing <| ignore_alter <| BonusProtectionSlashing >> the_msg
        , new_form_field_int "bonus_encumbrance" .bonus_encumbrance <| ignore_alter <| BonusEncumbrance >> the_msg
        , new_form_field_int "rarity_type" .rarity_type <| ignore_alter <| RarityType >> the_msg
        , new_form_field_int "carry_weight" .carry_weight <| ignore_alter <| CarryWeight >> the_msg
        ]
    }


type alias ArmorFrame =
    { pretty_name : String
    , frame_id : Int

    -- , description : String
    -- , frame_image_path : String
    , bonus_defense : Int
    , bonus_protection : Int
    , bonus_protection_piercing : Int
    , bonus_protection_blunt : Int
    , bonus_protection_slashing : Int
    , bonus_encumbrance : Int
    , rarity_type : Int
    , carry_weight : Int
    }