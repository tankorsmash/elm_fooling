module Torrent exposing (Model, Msg, init, update, view)

import Array
import Browser.Dom
import Browser.Events
import Color
import Color.Convert as Convert
import DOM exposing (offsetWidth, target)
import Dict
import Element
    exposing
        ( Color
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , explain
        , fill
        , fillPortion
        , height
        , modular
        , padding
        , paddingXY
        , paragraph
        , rgb
        , rgb255
        , row
        , scrollbars
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy as Lazy
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra
import Random
import Random.List
import Task
import Test exposing (..)
import Time
import Tuple3
import UUID exposing (UUID)


type alias Model =
    {}


type Msg
    = Noop


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Element.layoutWith { options = [] }
        []
    <|
        text "TORRENTS"
