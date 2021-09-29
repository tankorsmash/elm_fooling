module Magnolia.AttributeFrame exposing
    ( AttributeFrame
    , EditFormUpdateType
    , download_all_frames
    , edit_form_definition
    , update_edit_form_data
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
        , new_form_field_list_int
        , new_form_field_list_string
        , new_form_field_string
        , update_enum_field
        , update_int_field
        )
import Http
import Json.Decode as Decode exposing (Decoder, andThen, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Utils exposing (clojure_json_server_url, root_json_server_url)


decode_attribute_frame : Decoder AttributeFrame
decode_attribute_frame =
    succeed AttributeFrame
        |> required "frame_id" int
        |> required "pretty_name" string
        |> required "description" string
        |> required "frame_image_path" string
        |> required "state_names" (list string)
        |> required "state_names_pretty_funcs" (list string)
        |> required "pretty_name_template" string
        |> required "pretty_func_name" string


decode_attribute_frames : Decoder (List AttributeFrame)
decode_attribute_frames =
    list decode_attribute_frame


download_all_frames : (Utils.JsonHttpResult (List AttributeFrame) -> msg) -> Cmd msg
download_all_frames callback =
    Http.get
        { url = clojure_json_server_url ++ "api/frames/attribute"
        , expect = Http.expectJson callback (Utils.json_server_resp_decoder decode_attribute_frames)
        }


{-| All these are strings because they get the msg from Html
-}
type EditFormUpdateType
    = FrameId String
    | PrettyName String
    | Description String
    | FrameImagePath String
    | StateNames FieldAlterType String
    | StateNamesPrettyFuncs FieldAlterType String
    | PrettyNameTemplate String
    | PrettyFuncName String


update_edit_form_data : AttributeFrame -> EditFormUpdateType -> AttributeFrame
update_edit_form_data form_data form_update_type =
    case form_update_type of
        FrameId new_frame_id ->
            { form_data
                | frame_id = update_int_field form_data.frame_id new_frame_id
            }

        PrettyName new_name ->
            { form_data | pretty_name = new_name }

        Description new_description ->
            { form_data | description = new_description }

        FrameImagePath new_frame_image_path ->
            { form_data | frame_image_path = new_frame_image_path }

        StateNames alter_type new_state_names ->
            { form_data | state_names = String.split ", " new_state_names }

        StateNamesPrettyFuncs alter_type new_pretty_funcs ->
            { form_data | state_names_pretty_funcs = String.split ", " new_pretty_funcs }

        PrettyNameTemplate new_name_template ->
            { form_data | pretty_name_template = new_name_template }

        PrettyFuncName new_func_name ->
            { form_data | pretty_func_name = new_func_name }


edit_form_definition : (EditFormUpdateType -> msg) -> FormData.FormDefinition AttributeFrame msg
edit_form_definition the_msg =
    let
        _ =
            1
    in
    { fields =
        [ new_form_field_int "frame_id" .frame_id <| ignore_alter <| FrameId >> the_msg
        , new_form_field_string "pretty_name" .pretty_name <| ignore_alter <| PrettyName >> the_msg
        , new_form_field_string "description" .description <| ignore_alter <| Description >> the_msg
        , new_form_field_string "frame_image_path" .frame_image_path <| ignore_alter <| FrameImagePath >> the_msg
        , new_form_field_list_string "state_names" .state_names <| \at -> StateNames at >> the_msg
        , new_form_field_list_string "state_names_pretty_funcs" .state_names_pretty_funcs <| \at -> StateNamesPrettyFuncs at >> the_msg
        , new_form_field_string "pretty_name_template" .pretty_name_template <| ignore_alter <| PrettyNameTemplate >> the_msg
        , new_form_field_string "pretty_func_name" .pretty_func_name <| ignore_alter <| PrettyFuncName >> the_msg
        ]
    }


type alias AttributeFrame =
    { frame_id : Int
    , pretty_name : String
    , description : String
    , frame_image_path : String
    , state_names : List String
    , state_names_pretty_funcs : List String
    , pretty_name_template : String
    , pretty_func_name : String
    }
