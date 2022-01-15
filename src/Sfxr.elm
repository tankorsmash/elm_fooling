port module Sfxr exposing (Model, Msg(..), init, subscriptions, update, view)

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
import Html
import Html.Attributes
import Interface as UI
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Task
import Test exposing (..)
import Time


type Msg
    = Noop
    | PlaySound
    | FromPort String


type Shape
    = Square
    | Sawtooth
    | Sine
    | Noise


encodeShape : Shape -> Int
encodeShape shape =
    case shape of
        Square ->
            0

        Sawtooth ->
            1

        Sine ->
            2

        Noise ->
            3


decodeShape : Int -> Shape
decodeShape shapeInt =
    case shapeInt of
        0 ->
            Square

        1 ->
            Sawtooth

        2 ->
            Sine

        3 ->
            Noise

        _ ->
            Square


type alias SoundConfig =
    { shape : Shape
    }


encodeSoundConfig : SoundConfig -> Encode.Value
encodeSoundConfig soundConfig =
    Encode.object
        [ ( "wave_type", Encode.int <| encodeShape soundConfig.shape )
        ]


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
        Noop ->
            noop

        PlaySound ->
            ( model, sfxrOut "PlaySound sent" )

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


view : Model -> Html.Html Msg
view model =
    Element.layoutWith
        { options =
            [ Element.noStaticStyleSheet
            , Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ padding 20
        ]
    <|
        column []
            [ text "TEMP SFXR"
            , UI.primary_button [] PlaySound "Play"
            ]


port sfxrIn : (String -> msg) -> Sub msg


port sfxrOut : String -> Cmd msg
