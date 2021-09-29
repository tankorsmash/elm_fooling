module Magnolia.WeaponCategoryFrame exposing
    ( EditFormUpdateType
    , WeaponCategoryFrame
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
        , new_form_field_string
        , update_enum_field
        , update_int_field
        )
import Http
import Json.Decode as Decode exposing (Decoder, andThen, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Utils exposing (root_json_server_url, clojure_json_server_url)


decode_weapon_category_frame : Decoder WeaponCategoryFrame
decode_weapon_category_frame =
    succeed WeaponCategoryFrame
        |> required "frame_id" int
        |> required "pretty_name" string
        |> required "description" string
        |> required "frame_image_path" string
        |> required "rarity_type" int
        |> required "weapon_frame_ids" (list int)
        |> required "rank_1_attr_frame_ids" (list int)
        |> required "rank_2_attr_frame_ids" (list int)
        |> required "rank_3_attr_frame_ids" (list int)


decode_weapon_category_frames : Decoder (List WeaponCategoryFrame)
decode_weapon_category_frames =
    list decode_weapon_category_frame


download_all_frames : (Utils.JsonHttpResult (List WeaponCategoryFrame) -> msg) -> Cmd msg
download_all_frames callback =
    Http.get
        { url = clojure_json_server_url ++ "api/frames/weapon_category"
        , expect = Http.expectJson callback (Utils.json_server_resp_decoder decode_weapon_category_frames)
        }


{-| All these are strings because they get the msg from Html
-}
type EditFormUpdateType
    = FrameId String
    | PrettyName String
    | Description String
    | FrameImagePath String
    | RarityType String
    | WeaponFrameIds FieldAlterType String
    | Rank1AttrFrameIds FieldAlterType String
    | Rank2AttrFrameIds FieldAlterType String
    | Rank3AttrFrameIds FieldAlterType String


update_edit_form_data : WeaponCategoryFrame -> EditFormUpdateType -> WeaponCategoryFrame
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

        RarityType new_rarity_type ->
            { form_data | rarity_type = update_int_field form_data.rarity_type new_rarity_type }

        WeaponFrameIds alter_type new_frame_ids ->
            { form_data | weapon_frame_ids = List.map (Maybe.withDefault -1 << String.toInt) <| String.split ", " new_frame_ids }

        Rank1AttrFrameIds alter_type attr_frame_ids ->
            { form_data | rank_1_attr_frame_ids = List.map (Maybe.withDefault -1 << String.toInt) <| String.split ", " attr_frame_ids }

        Rank2AttrFrameIds alter_type attr_frame_ids ->
            { form_data | rank_2_attr_frame_ids = List.map (Maybe.withDefault -1 << String.toInt) <| String.split ", " attr_frame_ids }

        Rank3AttrFrameIds alter_type attr_frame_ids ->
            { form_data | rank_3_attr_frame_ids = List.map (Maybe.withDefault -1 << String.toInt) <| String.split ", " attr_frame_ids }


edit_form_definition : (EditFormUpdateType -> msg) -> FormData.FormDefinition WeaponCategoryFrame msg
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
        , new_form_field_int "rarity_type" .rarity_type <| ignore_alter <| RarityType >> the_msg
        , new_form_field_list_int "weapon_frame_ids" .weapon_frame_ids <| \at -> WeaponFrameIds at >> the_msg
        , new_form_field_list_int "rank_1_attr_frame_ids" .rank_1_attr_frame_ids <| \at -> Rank1AttrFrameIds at >> the_msg
        , new_form_field_list_int "rank_2_attr_frame_ids" .rank_2_attr_frame_ids <| \at -> Rank2AttrFrameIds at >> the_msg
        , new_form_field_list_int "rank_3_attr_frame_ids" .rank_3_attr_frame_ids <| \at -> Rank3AttrFrameIds at >> the_msg
        ]
    }


type alias WeaponCategoryFrame =
    { frame_id : Int
    , pretty_name : String
    , description : String
    , frame_image_path : String
    , rarity_type : Int
    , weapon_frame_ids : List Int
    , rank_1_attr_frame_ids : List Int
    , rank_2_attr_frame_ids : List Int
    , rank_3_attr_frame_ids : List Int
    }
