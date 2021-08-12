module Magnolia.ZoneFrame exposing
    ( EditFormUpdateType
    , ZoneFrame
    , edit_form_definition
    , update_edit_form_data
    )

import FormData
    exposing
        ( DataType(..)
        , ListFieldAlterType(..)
        , new_form_field_enum
        , new_form_field_float
        , new_form_field_int
        , new_form_field_list_string
        , new_form_field_string
        , update_enum_field
        , update_int_field
        )


type EditFormUpdateType
    = Name String
    | DataName String
    | RequiredZoneDataNameToUnlock String
    -- | LocationDataNamesInTheZone ListFieldAlterType String
    | LocationDataNamesInTheZone String


update_edit_form_data : ZoneFrame -> EditFormUpdateType -> ZoneFrame
update_edit_form_data form_data form_update_type =
    case form_update_type of
        Name new_name ->
            { form_data | name = new_name }

        DataName new_data_name ->
            { form_data | data_name = new_data_name }

        RequiredZoneDataNameToUnlock new_required_zone_data_name_to_unlock ->
            { form_data | required_zone_data_name_to_unlock = new_required_zone_data_name_to_unlock }

        -- LocationDataNamesInTheZone alter_type new_location_data_names_in_the_zone ->
        LocationDataNamesInTheZone new_location_data_names_in_the_zone ->
            { form_data | location_data_names_in_the_zone = String.split ", " new_location_data_names_in_the_zone }


edit_form_definition : (EditFormUpdateType -> msg) -> FormData.FormDefinition ZoneFrame msg
edit_form_definition the_msg =
    let
        -- location_msg : String -> ListFieldAlterType -> msg
        location_msg : String ->  msg
        location_msg =
            -- \str alter_type -> the_msg (LocationDataNamesInTheZone alter_type str)
            -- \str -> the_msg (Name str)
            \str -> the_msg (LocationDataNamesInTheZone str)

        location_field : FormData.FormField ZoneFrame msg
        location_field =
            new_form_field_list_string
                "location_data_names_in_the_zone"
                .location_data_names_in_the_zone
                location_msg
    in
    { fields =
        [ new_form_field_string "name" .name ((\str -> Name str) >> the_msg)
        , new_form_field_string "data_name" .data_name (DataName >> the_msg)
        , new_form_field_string "required_zone_data_name_to_unlock" .required_zone_data_name_to_unlock (RequiredZoneDataNameToUnlock >> the_msg)
        , location_field
        ]
    }


type alias ZoneFrame =
    { name : String
    , data_name : String
    , required_zone_data_name_to_unlock : String
    , location_data_names_in_the_zone : List String
    }
