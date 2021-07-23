module Reddit exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt, hardcoded)
import Result



-- temp because this'll point to reddit


root_json_server_url =
    "http://localhost:5021/"


type alias Thing a =
    { kind : String, data : List a }


type alias ListingWrapper =
    { kind : String, listing : Listing }

type alias Listing =
    { before : String, after : String, children : List SubmissionWrapper }

type alias SubmissionWrapper =
    { kind : String, data : Submission }

type alias Submission =
    { permalink : String
    , id : String
    , title : String
    , author : String
    , url : String
    }


download_reddit_posts : (Result Http.Error ListingWrapper -> msg) -> Cmd msg
download_reddit_posts the_msg =
    Http.get
        { url = root_json_server_url ++ "reddit_devblogs"
        , expect = Http.expectJson the_msg decode_listing_wrapper
        }


decode_submission_wrapper : Decoder SubmissionWrapper
decode_submission_wrapper =
    Decode.succeed SubmissionWrapper
        |> required "kind" string
        |> required "data" decode_submission

decode_submission : Decoder Submission
decode_submission =
    Decode.succeed Submission
        |> required "permalink" string
        |> required "id" string
        |> required "title" string
        |> required "author" string
        |> required "url" string


decode_listing_wrapper : Decoder ListingWrapper
decode_listing_wrapper =
    Decode.succeed ListingWrapper
        |> required "kind" string
        |> required "data" (decode_listing)


decode_listing : Decoder Listing
decode_listing =
    Decode.succeed Listing
        |> optional "before" string ""
        |> required "after" string
        |> required "children" (list decode_submission_wrapper)
