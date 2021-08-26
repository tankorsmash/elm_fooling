module Weather exposing (..)

import Api_keys exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, float, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)


type alias CurrentWeatherMain =
    { temp : Float
    , feels_like : Float
    , temp_min : Float
    , temp_max : Float
    , pressure : Int
    , humidity : Int
    }


type alias CurrentWeatherResponse =
    { name : String, main : CurrentWeatherMain }


current_weather_url area_str =
    String.concat
        [ "http://api.openweathermap.org/data/2.5/weather?q="
        , area_str

        -- , "Houston,TX,US"
        , "&appid="
        , open_weather_map_api_key
        , "&units="
        , "metric"
        ]



--temp that defaults to altstadt or something
-- current_weather_url = "https://samples.openweathermap.org/data/2.5/forecast?q=Gatineau,QC,CA&appid=439d4b804bc8187953eb36d2a8c26a02"


decode_current_weather_main : Decoder CurrentWeatherMain
decode_current_weather_main =
    Decode.succeed CurrentWeatherMain
        |> required "temp" float
        |> required "feels_like" float
        |> required "temp_min" float
        |> required "temp_max" float
        |> required "pressure" int
        |> required "humidity" int


decode_current_weather_response : Decoder CurrentWeatherResponse
decode_current_weather_response =
    Decode.succeed CurrentWeatherResponse
        |> required "name" string
        |> required "main" decode_current_weather_main


downloader : String -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
downloader area the_msg decoder =
    Http.get
        { url = current_weather_url area
        , expect = Http.expectJson the_msg decoder
        }


download_current_weather the_msg =
    downloader "Gatineau" the_msg decode_current_weather_response


download_current_areas_weather area_str the_msg =
    downloader area_str the_msg decode_current_weather_response
