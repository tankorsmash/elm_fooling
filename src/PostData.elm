module PostData exposing (PostData, decode_single)

import Json.Decode exposing (Decoder, at, field, list, string)
import Json.Encode exposing (string)


type alias PostData =
    { id : Int
    , title : String
    , author : String
    }


decode_single : Decoder PostData
decode_single =
    Json.Decode.map3 PostData
        (at [ "id" ] Json.Decode.int)
        (at [ "title" ] Json.Decode.string)
        (at [ "author" ] Json.Decode.string)
