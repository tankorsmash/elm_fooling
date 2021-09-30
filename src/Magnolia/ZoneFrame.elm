module Magnolia.ZoneFrame exposing
    ( EditFormUpdateType
    , ZoneFrame
    , download_all_frames
    , edit_form_definition
    , encode_zone_frame
    , update_edit_form_data
    )

import FormData
    exposing
        ( DataType(..)
        , FieldAlterType(..)
        , InputCallback
        , ignore_alter
        , new_form_field_enum
        , new_form_field_float
        , new_form_field_int
        , new_form_field_list_string
        , new_form_field_string
        , update_enum_field
        , update_int_field
        )
import Http
import Json.Decode as Decode exposing (Decoder, andThen, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Json.Encode as Encode
import Utils exposing (clojure_json_server_url, root_json_server_url)


decode_zone_frame : Decoder ZoneFrame
decode_zone_frame =
    succeed ZoneFrame
        |> required "name" string
        |> required "data_name" string
        |> required "required_zone_data_name_to_unlock" string
        |> required "location_data_names_in_the_zone" (list string)


decode_zone_frames : Decoder (List ZoneFrame)
decode_zone_frames =
    list decode_zone_frame


encode_zone_frame : ZoneFrame -> Encode.Value
encode_zone_frame frame_data =
    Encode.object
        [ ( "name", Encode.string frame_data.name )
        , ( "data_name", Encode.string frame_data.data_name )
        , ( "required_zone_data_name_to_unlock", Encode.string frame_data.required_zone_data_name_to_unlock )
        , ( "location_data_names_in_the_zone", Encode.list Encode.string frame_data.location_data_names_in_the_zone )
        ]


download_all_frames : (Utils.JsonHttpResult (List ZoneFrame) -> msg) -> Cmd msg
download_all_frames callback =
    Http.get
        { url = clojure_json_server_url ++ "api/frames/zone"
        , expect = Http.expectJson callback (Utils.json_server_resp_decoder decode_zone_frames)
        }


type EditFormUpdateType
    = Name String
    | DataName String
    | RequiredZoneDataNameToUnlock String
    | LocationDataNamesInTheZone FieldAlterType String


update_edit_form_data : ZoneFrame -> EditFormUpdateType -> ZoneFrame
update_edit_form_data form_data form_update_type =
    case form_update_type of
        Name new_name ->
            { form_data | name = new_name }

        DataName new_data_name ->
            { form_data | data_name = new_data_name }

        RequiredZoneDataNameToUnlock new_required_zone_data_name_to_unlock ->
            { form_data | required_zone_data_name_to_unlock = new_required_zone_data_name_to_unlock }

        LocationDataNamesInTheZone alter_type new_location_data_names_in_the_zone ->
            { form_data | location_data_names_in_the_zone = String.split ", " new_location_data_names_in_the_zone }


edit_form_definition : (EditFormUpdateType -> msg) -> FormData.FormDefinition ZoneFrame msg
edit_form_definition the_msg =
    let
        location_field : FormData.FormField ZoneFrame msg
        location_field =
            new_form_field_list_string
                "location_data_names_in_the_zone"
                .location_data_names_in_the_zone
            <|
                \alter_type -> LocationDataNamesInTheZone alter_type >> the_msg
    in
    { fields =
        [ new_form_field_string "name" .name <| ignore_alter ((\str -> Name str) >> the_msg)
        , new_form_field_string "data_name" .data_name <| ignore_alter (DataName >> the_msg)
        , new_form_field_string "required_zone_data_name_to_unlock" .required_zone_data_name_to_unlock (ignore_alter <| (RequiredZoneDataNameToUnlock >> the_msg))
        , location_field
        ]
    }


type alias ZoneFrame =
    { name : String
    , data_name : String
    , required_zone_data_name_to_unlock : String
    , location_data_names_in_the_zone : List String
    }
