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


encodeShape : Shape -> ( String, Encode.Value )
encodeShape shape =
    ( "wave_type"
    , Encode.int <|
        case shape of
            Square ->
                0

            Sawtooth ->
                1

            Sine ->
                2

            Noise ->
                3
    )


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


type alias Envelope =
    { attack : Float
    , sustain : Float
    , punch : Float
    , decay : Float
    }


encodeEnvelope : Envelope -> List ( String, Encode.Value )
encodeEnvelope envelope =
    [ ( "p_env_attack", Encode.float envelope.attack )
    , ( "p_env_sustain", Encode.float envelope.sustain )
    , ( "p_env_punch", Encode.float envelope.punch )
    , ( "p_env_decay", Encode.float envelope.decay )
    ]


decodeEnvelope : Encode.Value -> Decoder Envelope
decodeEnvelope obj =
    Decode.map4 Envelope
        (Decode.field "p_env_attack" Decode.float)
        (Decode.field "p_env_sustain" Decode.float)
        (Decode.field "p_env_punch" Decode.float)
        (Decode.field "p_env_decay" Decode.float)


type alias Frequency =
    { base : Float
    , limit : Float
    , ramp : Float
    , dramp : Float
    }


encodeFrequency : Frequency -> List ( String, Encode.Value )
encodeFrequency frequency =
    [ ( "p_freq_base_freq", Encode.float frequency.base )
    , ( "p_freq_limit", Encode.float frequency.limit )
    , ( "p_freq_ramp", Encode.float frequency.ramp )
    , ( "p_freq_dramp", Encode.float frequency.dramp )
    ]


decodeFrequency : Encode.Value -> Decoder Frequency
decodeFrequency obj =
    Decode.map4 Frequency
        (Decode.field "p_freq_base_freq" Decode.float)
        (Decode.field "p_freq_limit" Decode.float)
        (Decode.field "p_freq_ramp" Decode.float)
        (Decode.field "p_freq_dramp" Decode.float)


type alias Vibrato =
    { strength : Float
    , speed : Float
    }


encodeVibrato : Vibrato -> List ( String, Encode.Value )
encodeVibrato vibrato =
    [ ( "p_vib_strength", Encode.float vibrato.strength )
    , ( "p_vib_speed", Encode.float vibrato.speed )
    ]


decodeVibrato : Encode.Value -> Decoder Vibrato
decodeVibrato obj =
    Decode.map2 Vibrato
        (Decode.field "p_vib_strength" Decode.float)
        (Decode.field "p_vib_speed" Decode.float)


type alias Arpeggiation =
    { mod : Float
    , speed : Float
    }


encodeArpeggiation : Arpeggiation -> List ( String, Encode.Value )
encodeArpeggiation arpeggiation =
    [ ( "p_arp_mod", Encode.float arpeggiation.mod )
    , ( "p_arp_speed", Encode.float arpeggiation.speed )
    ]


decodeArpeggiation : Encode.Value -> Decoder Arpeggiation
decodeArpeggiation obj =
    Decode.map2 Arpeggiation
        (Decode.field "p_arp_mod" Decode.float)
        (Decode.field "p_arp_speed" Decode.float)

type alias Duty =
    { duty : Float
    , ramp : Float
    }


encodeDuty : Duty -> List ( String, Encode.Value )
encodeDuty duty =
    [ ( "p_duty", Encode.float duty.duty )
    , ( "p_duty_ramp", Encode.float duty.ramp )
    ]


decodeDuty : Encode.Value -> Decoder Duty
decodeDuty obj =
    Decode.map2 Duty
        (Decode.field "p_duty" Decode.float)
        (Decode.field "p_duty_ramp" Decode.float)


type alias SoundConfig =
    { shape : Shape
    , envelope : Envelope
    , frequency : Frequency
    , vibrato : Vibrato
    , arpeggiation : Arpeggiation
    , duty : Duty
    }


encodeSoundConfig : SoundConfig -> Encode.Value
encodeSoundConfig soundConfig =
    Encode.object
        ([ encodeShape soundConfig.shape
         ]
            ++ encodeEnvelope soundConfig.envelope
            ++ encodeFrequency soundConfig.frequency
            ++ encodeVibrato soundConfig.vibrato
            ++ encodeArpeggiation soundConfig.arpeggiation
            ++ encodeDuty soundConfig.duty
        )



-- decodeSoundConfig : Decode.Value -> SoundConfig


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
