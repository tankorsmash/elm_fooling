module Magnolia.WeaponFrame exposing (EditFormUpdateType, WeaponFrame, edit_form_definition, update_edit_form_data)

import FormData
    exposing
        ( DataType(..)
        , new_form_field_float
        , new_form_field_int
        , new_form_field_string
        )


type EditFormUpdateType
    = Name String
    | FrameId String
    | ChoiceId String


update_edit_form_data : WeaponFrame -> EditFormUpdateType -> WeaponFrame
update_edit_form_data form_data form_update_type =
    case form_update_type of
        Name new_name ->
            { form_data | weapon_name = new_name }

        ChoiceId new_choice_id ->
            { form_data
                | choice_id =
                    case String.toInt new_choice_id of
                        Just new_int ->
                            new_int

                        Nothing ->
                            form_data.choice_id
            }

        FrameId new_frame_id ->
            { form_data
                | frame_id =
                    case String.toInt new_frame_id of
                        Just new_int ->
                            new_int

                        Nothing ->
                            form_data.frame_id
            }


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
        ]
    }


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
