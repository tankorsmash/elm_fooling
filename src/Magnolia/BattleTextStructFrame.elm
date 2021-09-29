module Magnolia.BattleTextStructFrame exposing
    ( BattleTextStructFrame
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
import Utils exposing (root_json_server_url, clojure_json_server_url)


decode_battle_text_struct_frame : Decoder BattleTextStructFrame
decode_battle_text_struct_frame =
    succeed BattleTextStructFrame
        |> required "frame_id" int
        |> required "pretty_name" string
        |> required "description" string
        |> required "battle_won_text_fid" int
        |> required "battle_tied_text_fid" int
        |> required "battle_lost_text_fid" int


decode_battle_text_struct_frames : Decoder (List BattleTextStructFrame)
decode_battle_text_struct_frames =
    list decode_battle_text_struct_frame


download_all_frames : (Result Http.Error (List BattleTextStructFrame) -> msg) -> Cmd msg
download_all_frames callback =
    Http.get
        { url = root_json_server_url ++ "api/frames/battle_text_struct"
        , expect = Http.expectJson callback decode_battle_text_struct_frames
        }


{-| All these are strings because they get the msg from Html
-}
type EditFormUpdateType
    = FrameId String
    | PrettyName String
    | Description String
    | BattleWonTextFID String
    | BattleTiedTextFID String
    | BattleLostTextFID String


update_edit_form_data : BattleTextStructFrame -> EditFormUpdateType -> BattleTextStructFrame
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

        BattleWonTextFID new_battle_won_text_fid ->
            { form_data | battle_won_text_fid = update_int_field form_data.battle_won_text_fid new_battle_won_text_fid }

        BattleTiedTextFID new_battle_tied_text_fid ->
            { form_data | battle_tied_text_fid = update_int_field form_data.battle_tied_text_fid new_battle_tied_text_fid }

        BattleLostTextFID new_battle_lost_text_fid ->
            { form_data | battle_lost_text_fid = update_int_field form_data.battle_lost_text_fid new_battle_lost_text_fid }


edit_form_definition : (EditFormUpdateType -> msg) -> FormData.FormDefinition BattleTextStructFrame msg
edit_form_definition the_msg =
    let
        _ =
            1
    in
    { fields =
        [ new_form_field_int "frame_id" .frame_id <| ignore_alter <| FrameId >> the_msg
        , new_form_field_string "pretty_name" .pretty_name <| ignore_alter <| PrettyName >> the_msg
        , new_form_field_string "description" .description <| ignore_alter <| Description >> the_msg
        , new_form_field_int "battle_won_text_fid" .battle_won_text_fid <| ignore_alter <| BattleWonTextFID >> the_msg
        , new_form_field_int "battle_tied_text_fid" .battle_tied_text_fid <| ignore_alter <| BattleTiedTextFID >> the_msg
        , new_form_field_int "battle_lost_text_fid" .battle_lost_text_fid <| ignore_alter <| BattleLostTextFID >> the_msg
        ]
    }


type alias BattleTextStructFrame =
    { frame_id : Int
    , pretty_name : String
    , description : String
    , battle_won_text_fid : Int
    , battle_tied_text_fid : Int
    , battle_lost_text_fid : Int
    }
