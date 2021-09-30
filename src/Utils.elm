module Utils exposing (..)

import Html.Attributes exposing (attribute, href, property, style)
import Http
import Json.Decode as Decode
import Json.Encode


add_class cls =
    property "className" (Json.Encode.string cls)


root_json_server_url =
    "http://localhost:5021/"


clojure_json_server_url =
    "http://localhost:9500/"


{-| Helper around Http results
-}
type alias JsonHttpResult a =
    Result Http.Error (JsonServerResp a)

type alias JsonHttpResultString =
    Result Http.Error (JsonServerResp String)


type alias JsonServerResp a =
    { success : Bool
    , message : String
    , data : a
    }


json_server_resp_decoder_string : Decode.Decoder (JsonServerResp String)
json_server_resp_decoder_string =
    Decode.map3 JsonServerResp
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)
        (Decode.field "data" Decode.string)

json_server_resp_decoder : Decode.Decoder a -> Decode.Decoder (JsonServerResp a)
json_server_resp_decoder data_decoder =
    Decode.map3 JsonServerResp
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)
        (Decode.field "data" data_decoder)
