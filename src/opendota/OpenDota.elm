module OpenDota.OpenDota exposing (..)

import Http

root_url : String
root_url = "https://api.opendota.com/api/"

download_subreddit_posts : String -> (Result Http.Error ListingWrapper -> msg) -> Cmd msg
download_subreddit_posts subreddit the_msg =
    Http.get
        { url = root_reddit_url ++ "r/" ++ subreddit ++ "/.json?jsonp=jsonpCallback"
        , expect = Http.expectJson the_msg decode_listing_wrapper
        }
