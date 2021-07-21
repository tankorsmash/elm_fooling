module Weather exposing (..)

import Api_keys exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)


type alias CurrentWeatherResponse =
    { name : String }


current_weather_url =
    String.concat
        [ "http://api.openweathermap.org/data/2.5/weather?q="
        , open_weather_city_code
        , "&appid="
        , open_weather_map_api_key
        ]



--temp that defaults to altstadt or something
-- current_weather_url = "https://samples.openweathermap.org/data/2.5/forecast?q=Gatineau,QC,CA&appid=439d4b804bc8187953eb36d2a8c26a02"


decode_current_weather_response : Decoder CurrentWeatherResponse
decode_current_weather_response =
    Decode.succeed CurrentWeatherResponse
        |> required "name" string

downloader : (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
downloader the_msg decoder =
    Http.get
        { url = current_weather_url
        , expect = Http.expectJson the_msg decoder
        }

download_current_weather the_msg =
        downloader the_msg decode_current_weather_response
