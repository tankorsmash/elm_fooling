module Reddit exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Result



-- temp because this'll point to reddit


root_json_server_url =
    "http://localhost:5021/"


type alias Thing a =
    { kind : String, data : List a }


type alias Listing =
    { before : String, after : String, children : List Submission }


type alias Submission =
    { kind : String
    , permalink : String
    , id : String
    , title : String
    , author : String
    }


type Msg
    = DownloadedRedditPosts (Result Http.Error Listing)

-- download_reddit_posts : c Result a b -> Cmd msg
-- download_reddit_posts param_msg =

download_reddit_posts : Cmd Msg
download_reddit_posts =
    Http.get
        { url = root_json_server_url ++ "reddit_devblogs"
        , expect = Http.expectJson DownloadedRedditPosts decode_listing
        }

-- custom_download_reddit_posts : msg -> Cmd msg
custom_download_reddit_posts the_msg =
    Http.get
        { url = root_json_server_url ++ "reddit_devblogs"
        , expect = Http.expectJson the_msg decode_listing
        }


decode_submission : Decoder Submission
decode_submission =
    Decode.succeed Submission
        |> required "kind" string
        |> required "permalink" string
        |> required "id" string
        |> required "title" string
        |> required "author" string


decode_listing : Decoder Listing
decode_listing =
    Decode.succeed Listing
        |> required "before" string
        |> required "after" string
        |> required "children" (list decode_submission)
