port module Sfxr exposing (Model, Msg(..), init, subscriptions, suite, update, view)

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


decodeEnvelope : Decoder Envelope
decodeEnvelope =
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
    [ ( "p_base_freq", Encode.float frequency.base )
    , ( "p_freq_limit", Encode.float frequency.limit )
    , ( "p_freq_ramp", Encode.float frequency.ramp )
    , ( "p_freq_dramp", Encode.float frequency.dramp )
    ]


decodeFrequency : Decoder Frequency
decodeFrequency =
    Decode.map4 Frequency
        (Decode.field "p_base_freq" Decode.float)
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


decodeVibrato : Decoder Vibrato
decodeVibrato =
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


decodeArpeggiation : Decoder Arpeggiation
decodeArpeggiation =
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


decodeDuty : Decoder Duty
decodeDuty =
    Decode.map2 Duty
        (Decode.field "p_duty" Decode.float)
        (Decode.field "p_duty_ramp" Decode.float)


type alias Retrigger =
    { repeatSpeed : Float
    }


encodeRetrigger : Retrigger -> List ( String, Encode.Value )
encodeRetrigger retrigger =
    [ ( "p_repeat_speed", Encode.float retrigger.repeatSpeed )
    ]


decodeRetrigger : Decoder Retrigger
decodeRetrigger =
    Decode.map Retrigger
        (Decode.field "p_repeat_speed" Decode.float)


type alias Flanger =
    { offset : Float
    , ramp : Float
    }


encodeFlanger : Flanger -> List ( String, Encode.Value )
encodeFlanger flanger =
    [ ( "p_pha_offset", Encode.float flanger.offset )
    , ( "p_pha_ramp", Encode.float flanger.ramp )
    ]


decodeFlanger : Decoder Flanger
decodeFlanger =
    Decode.map2 Flanger
        (Decode.field "p_pha_offset" Decode.float)
        (Decode.field "p_pha_ramp" Decode.float)


type alias LowPassFilter =
    { frequency : Float
    , ramp : Float
    , resonance : Float
    }


encodeLowPassFilter : LowPassFilter -> List ( String, Encode.Value )
encodeLowPassFilter lowPassFilter =
    [ ( "p_lpf_freq", Encode.float lowPassFilter.frequency )
    , ( "p_lpf_ramp", Encode.float lowPassFilter.ramp )
    , ( "p_lpf_resonance", Encode.float lowPassFilter.resonance )
    ]


decodeLowPassFilter : Decoder LowPassFilter
decodeLowPassFilter =
    Decode.map3 LowPassFilter
        (Decode.field "p_lpf_freq" Decode.float)
        (Decode.field "p_lpf_ramp" Decode.float)
        (Decode.field "p_lpf_resonance" Decode.float)


type alias HighPassFilter =
    { frequency : Float
    , ramp : Float
    }


encodeHighPassFilter : HighPassFilter -> List ( String, Encode.Value )
encodeHighPassFilter highPassFilter =
    [ ( "p_hpf_freq", Encode.float highPassFilter.frequency )
    , ( "p_hpf_ramp", Encode.float highPassFilter.ramp )
    ]


decodeHighPassFilter : Decoder HighPassFilter
decodeHighPassFilter =
    Decode.map2 HighPassFilter
        (Decode.field "p_hpf_freq" Decode.float)
        (Decode.field "p_hpf_ramp" Decode.float)


type alias Misc =
    { volume : Float
    , sampleRate : Int
    , sampleSize : Int
    }


encodeMisc : Misc -> List ( String, Encode.Value )
encodeMisc misc =
    [ ( "sound_vol", Encode.float misc.volume )
    , ( "sample_rate", Encode.int misc.sampleRate )
    , ( "sample_size", Encode.int misc.sampleSize )
    ]


decodeMisc : Decoder Misc
decodeMisc =
    Decode.map3 Misc
        (Decode.field "sound_vol" Decode.float)
        (Decode.field "sample_rate" Decode.int)
        (Decode.field "sample_size" Decode.int)


type alias SoundConfig =
    { shape : Shape
    , envelope : Envelope
    , frequency : Frequency
    , vibrato : Vibrato
    , arpeggiation : Arpeggiation
    , duty : Duty
    , retrigger : Retrigger
    , flanger : Flanger
    , lowPassFilter : LowPassFilter
    , highPassFilter : HighPassFilter
    , misc : Misc
    }


encodeSoundConfig : SoundConfig -> Encode.Value
encodeSoundConfig soundConfig =
    Encode.object
        ([ ( "oldParams", Encode.bool True ) ]
            ++ [ encodeShape soundConfig.shape ]
            ++ encodeEnvelope soundConfig.envelope
            ++ encodeFrequency soundConfig.frequency
            ++ encodeVibrato soundConfig.vibrato
            ++ encodeArpeggiation soundConfig.arpeggiation
            ++ encodeDuty soundConfig.duty
            ++ encodeRetrigger soundConfig.retrigger
            ++ encodeFlanger soundConfig.flanger
            ++ encodeLowPassFilter soundConfig.lowPassFilter
            ++ encodeHighPassFilter soundConfig.highPassFilter
            ++ encodeMisc soundConfig.misc
        )


type alias PartialSoundConfigA =
    -- { shape : Shape
    { shape : Int
    , envelope : Envelope
    , frequency : Frequency
    , vibrato : Vibrato
    , arpeggiation : Arpeggiation
    , duty : Duty
    , retrigger : Retrigger
    , flanger : Flanger

    -- , lowPassFilter : LowPassFilter
    -- , highPassFilter : HighPassFilter
    -- , misc : Misc
    }


type alias PartialSoundConfigB =
    -- { shape : Shape
    -- { shape : Int
    -- , envelope : Envelope
    -- , frequency : Frequency
    -- , vibrato : Vibrato
    -- , arpeggiation : Arpeggiation
    -- , duty : Duty
    -- , retrigger : Retrigger
    -- , flanger : Flanger
    { lowPassFilter : LowPassFilter
    , highPassFilter : HighPassFilter
    , misc : Misc
    }


decodeSoundConfigA : Decoder PartialSoundConfigA
decodeSoundConfigA =
    Decode.map8 PartialSoundConfigA
        -- ((Decode.field "wave_type" (Decode.int)) |> Decode.andThen decodeShape)
        (Decode.field "wave_type" Decode.int)
        decodeEnvelope
        decodeFrequency
        decodeVibrato
        decodeArpeggiation
        decodeDuty
        decodeRetrigger
        decodeFlanger


decodeSoundConfigB : Decoder PartialSoundConfigB
decodeSoundConfigB =
    Decode.map3 PartialSoundConfigB
        decodeLowPassFilter
        decodeHighPassFilter
        decodeMisc


decodeSoundConfig : Decoder SoundConfig
decodeSoundConfig =
    Decode.map2 combinePartialsIntoSoundConfig
        decodeSoundConfigA
        decodeSoundConfigB


combinePartialsIntoSoundConfig : PartialSoundConfigA -> PartialSoundConfigB -> SoundConfig
combinePartialsIntoSoundConfig partialA partialB =
    { -- { shape : Shape
      --partialA
      shape = partialA.shape |> decodeShape
    , envelope = partialA.envelope
    , frequency = partialA.frequency
    , vibrato = partialA.vibrato
    , arpeggiation = partialA.arpeggiation
    , duty = partialA.duty
    , retrigger = partialA.retrigger
    , flanger = partialA.flanger

    --partialB
    , lowPassFilter = partialB.lowPassFilter
    , highPassFilter = partialB.highPassFilter
    , misc = partialB.misc
    }


type alias Model =
    { soundConfig : SoundConfig }


init : Model
init =
    { soundConfig = initSoundConfig }


update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        Noop ->
            noop

        PlaySound ->
            ( model, sfxrOut <| encodeSoundConfig model.soundConfig )

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


port sfxrOut : Encode.Value -> Cmd msg


rawSampleSoundConfig : String
rawSampleSoundConfig =
    """{
    "oldParams": true,
    "wave_type": 1,
    "p_env_attack": 0,
    "p_env_sustain": 0.31718502829007483,
    "p_env_punch": 0,
    "p_env_decay": 0.2718540993592685,
    "p_base_freq": 0.26126191208337196,
    "p_freq_limit": 0,
    "p_freq_ramp": 0.43787689856926615,
    "p_freq_dramp": 0,
    "p_vib_strength": 0,
    "p_vib_speed": 0,
    "p_arp_mod": 0,
    "p_arp_speed": 0,
    "p_duty": 1,
    "p_duty_ramp": 0,
    "p_repeat_speed": 0.7558565452384385,
    "p_pha_offset": 0,
    "p_pha_ramp": 0,
    "p_lpf_freq": 1,
    "p_lpf_ramp": 0,
    "p_lpf_resonance": 0,
    "p_hpf_freq": 0,
    "p_hpf_ramp": 0,
    "sound_vol": 0.05,
    "sample_rate": 44100,
    "sample_size": 8
}"""
        |> --replace \r
           String.replace "\u{000D}" ""


initSoundConfig : SoundConfig
initSoundConfig =
    { shape = Square
    , envelope =
        { attack = 0
        , sustain = 0.3
        , punch = 0
        , decay = 0.4
        }
    , frequency =
        { base = 0.3
        , limit = 0
        , ramp = 0.0
        , dramp = 0
        }
    , vibrato = { strength = 0, speed = 0 }
    , arpeggiation = { mod = 0, speed = 0 }
    , duty = { duty = 0, ramp = 0 }
    , retrigger = { repeatSpeed = 0.0 }
    , flanger = { offset = 0, ramp = 0 }
    , lowPassFilter = { frequency = 1, ramp = 0, resonance = 0 }
    , highPassFilter = { frequency = 0, ramp = 0 }
    , misc = { volume = 0.05, sampleRate = 44100, sampleSize = 8 }
    }



expectedSoundConfig : SoundConfig
expectedSoundConfig =
    { shape = Sawtooth
    , envelope =
        { attack = 0
        , sustain = 0.31718502829007483
        , punch = 0
        , decay = 0.2718540993592685
        }
    , frequency =
        { base = 0.26126191208337196
        , limit = 0
        , ramp = 0.43787689856926615
        , dramp = 0
        }
    , vibrato = { strength = 0, speed = 0 }
    , arpeggiation = { mod = 0, speed = 0 }
    , duty = { duty = 1, ramp = 0 }
    , retrigger = { repeatSpeed = 0.7558565452384385 }
    , flanger = { offset = 0, ramp = 0 }
    , lowPassFilter = { frequency = 1, ramp = 0, resonance = 0 }
    , highPassFilter = { frequency = 0, ramp = 0 }
    , misc = { volume = 0.05, sampleRate = 44100, sampleSize = 8 }
    }


suite : Test
suite =
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "sfxr tests"
        [ describe "Encode/Decoding"
            [ test "Encodes as expect" <|
                \_ ->
                    let
                        encoded =
                            Encode.encode 4 (encodeSoundConfig expectedSoundConfig)
                    in
                    Expect.equal rawSampleSoundConfig encoded
            , test "Decodes Misc as you'd expect" <|
                \_ ->
                    let
                        decodedResult : Result Decode.Error Misc
                        decodedResult =
                            -- Decode.decodeString decodeSoundConfig rawSampleSoundConfig
                            Decode.decodeString decodeMisc rawSampleSoundConfig
                    in
                    case decodedResult of
                        Ok decoded ->
                            Expect.equal decoded expectedSoundConfig.misc

                        Err err ->
                            Expect.fail <| Decode.errorToString err
            , test "Decodes PartialSoundConfigA as you'd expect" <|
                \_ ->
                    let
                        decodedResult : Result Decode.Error SoundConfig
                        decodedResult =
                            Decode.decodeString decodeSoundConfig rawSampleSoundConfig
                    in
                    case decodedResult of
                        Ok decoded ->
                            -- Expect.equal decoded expectedSoundConfig.misc
                            Expect.equal decoded expectedSoundConfig

                        Err err ->
                            Expect.fail <| Decode.errorToString err
            ]
        ]
