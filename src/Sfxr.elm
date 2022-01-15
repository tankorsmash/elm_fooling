port module Sfxr exposing (Model, Msg(..), init, subscriptions, update)

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
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Task
import Test exposing (..)
import Time


type Msg
    = PlaySound
    | FromPort String


type alias Model =
    {}


init : Model
init =
    {}


update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        PlaySound ->
            noop

        FromPort str ->
            let
                _ =
                    Debug.log "from port" str
            in
            noop


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ --Time.every 1000 Tick,
          sfxrIn FromPort
        ]


port sfxrIn : (String -> msg) -> Sub msg


port sfxrOut : String -> Cmd msg
