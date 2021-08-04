module OpenDota.OpenDota exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Result

root_json_server_url =
    "http://localhost:5021/"

root_url : String
root_url =
    "https://api.opendota.com/api/"


type alias PlayerProfile =
    { account_id : Int
    , personaname : String
    , name : String
    , avatar : String
    , avatarfull : String
    }


type alias PlayerData =
    { profile : PlayerProfile }


decode_player_profile : Decoder PlayerProfile
decode_player_profile =
    Decode.succeed PlayerProfile
        |> required "account_id" int
        |> required "personaname" string
        |> optional "name" string "unset name"
        |> required "avatar" string
        |> required "avatarfull" string


decode_player_data : Decoder PlayerData
decode_player_data =
    Decode.succeed PlayerData
        |> required "profile" decode_player_profile


download_player_data : (Int) -> (Result Http.Error PlayerData -> msg) -> Cmd msg
download_player_data account_id the_msg =
    Http.get
        -- { url = root_url ++ "players/" ++ String.fromInt account_id
        { url = root_json_server_url ++ "open_dota_player_data"
        , expect = Http.expectJson the_msg decode_player_data
        }
