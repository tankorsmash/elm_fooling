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


type alias MMREstimate =
    { estimate : Int }


type alias PlayerProfile =
    { account_id : Int
    , personaname : String
    , name : String
    , avatar : String
    , avatarfull : String
    }


type alias PlayerData =
    { profile : PlayerProfile
    , mmr_estimate : MMREstimate
    }


decode_mmr_estimate : Decoder MMREstimate
decode_mmr_estimate =
    Decode.succeed MMREstimate
        |> required "estimate" int


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
        |> required "mmr_estimate" decode_mmr_estimate


download_player_data : Int -> (Result Http.Error PlayerData -> msg) -> Cmd msg
download_player_data account_id the_msg =
    let
        url =
            if account_id == 24801519 then
                root_json_server_url ++ "open_dota_player_data"

            else
                root_url ++ "players/" ++ String.fromInt account_id
    in
    Http.get
        { url = url
        , expect = Http.expectJson the_msg decode_player_data
        }


type alias HeroStat = { id: Int }

decode_hero_stat : Decoder HeroStat
decode_hero_stat =
    Decode.succeed HeroStat
        |> required "id" int


decode_hero_stats : Decoder (List HeroStat)
decode_hero_stats =
    list decode_hero_stat
    -- Decode.succeed (List HeroStat)
    --     |> 

download_hero_stats : (Result Http.Error (List HeroStat) -> msg) -> Cmd msg
download_hero_stats the_msg =
    let
        url =
            root_url ++ "heroStats"
    in
    Http.get
        { url = url
        , expect = Http.expectJson the_msg decode_hero_stats
        }
