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
import Random
import Random.List
import Task
import Test exposing (..)
import Time


type ShapeUpdateType
    = ShapeUpdateType Shape


type EnvelopeUpdateType
    = EnvAttack Float
    | EnvSustain Float
    | EnvPunch Float
    | EnvDecay Float


type FrequencyUpdateType
    = FrqBase Float
    | FrqLimit Float
    | FrqRamp Float
    | FrqDramp Float


type VibratoUpdateType
    = VibStrength Float
    | VibSpeed Float


type ArpeggiationUpdateType
    = ArpMod Float
    | ArpSpeed Float


type DutyUpdateType
    = DtyDuty Float
    | DtyRamp Float


type RetriggerUpdateType
    = RetRepeatSpeed Float


type FlangerUpdateType
    = FlaOffset Float
    | FlaRamp Float


type LowPassFilterUpdateType
    = LpfFrequency Float
    | LpfRamp Float
    | LpfResonance Float


type HighPassFilterUpdateType
    = HpfFrequency Float
    | HpfRamp Float


type MiscUpdateType
    = MscVolume Float
    | MscSampleRate Int
    | MscSampleSize Int


type ConfigType
    = ShapeConfigType ShapeUpdateType
    | EnvelopeConfigType EnvelopeUpdateType
    | FrequencyConfigType FrequencyUpdateType
    | VibratoConfigType VibratoUpdateType
    | ArpeggiationConfigType ArpeggiationUpdateType
    | DutyConfigType DutyUpdateType
    | RetriggerConfigType RetriggerUpdateType
    | FlangerConfigType FlangerUpdateType
    | LowPassFilterConfigType LowPassFilterUpdateType
    | HighPassFilterConfigType HighPassFilterUpdateType
    | MiscConfigType MiscUpdateType


type Msg
    = Noop
    | PlaySound
    | FromPort String
    | OnSliderChanged ConfigType
    | SetHitHurt
    | SetUpgrade
    | SetCoinPickup


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


setFreqBase : Float -> Frequency -> Frequency
setFreqBase newBase frequency =
    { frequency | base = newBase }


setFreqRamp : Float -> Frequency -> Frequency
setFreqRamp newRamp frequency =
    { frequency | ramp = newRamp }


setShape : Shape -> SoundConfig -> SoundConfig
setShape newShape sc =
    { sc | shape = newShape }


setRetriggerRepeatSpeed : Float -> Retrigger -> Retrigger
setRetriggerRepeatSpeed repeatSpeed retrigger =
    { retrigger | repeatSpeed = repeatSpeed }


setEnvAttack : Float -> Envelope -> Envelope
setEnvAttack attack envelope =
    { envelope | attack = attack }


setEnvSustain : Float -> Envelope -> Envelope
setEnvSustain sustain envelope =
    { envelope | sustain = sustain }


setEnvDecay : Float -> Envelope -> Envelope
setEnvDecay decay envelope =
    { envelope | decay = decay }


setEnvPunch : Float -> Envelope -> Envelope
setEnvPunch punch envelope =
    { envelope | punch = punch }


setDutyDuty : Float -> Duty -> Duty
setDutyDuty newDuty duty =
    { duty | duty = newDuty }


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
    { soundConfig : SoundConfig, globalSeed : Random.Seed, hideEmptySliders : Bool }


init : Model
init =
    { soundConfig = initSoundConfig
    , globalSeed = Random.initialSeed 12345
    , hideEmptySliders = False
    }



-- frnd : Float -> Random.Seed -> ( Float, Random.Seed )
-- frnd max floatSeed =
--     Random.step (Random.float 0 max) floatSeed


getFloat : Float -> Random.Generator Float
getFloat max =
    Random.float 0 max


getFloatRange : Float -> Float -> Random.Generator Float
getFloatRange min max =
    Random.float min max


flipCoin : Random.Generator Bool
flipCoin =
    Random.map (\n -> n < 50) (Random.int 1 100)


chooseRandomShape : List Shape -> Random.Generator Shape
chooseRandomShape choices =
    Random.List.choose choices
        |> Random.andThen
            (Tuple.first
                >> Maybe.withDefault Square
                >> Random.constant
            )


getRandomShape : Random.Generator Shape
getRandomShape =
    Random.List.choose [ Square, Sawtooth, Sine, Noise ]
        |> Random.andThen
            (Tuple.first
                >> Maybe.withDefault Square
                >> Random.constant
            )


getRandomHitHurt : Random.Seed -> ( SoundConfig, Random.Seed )
getRandomHitHurt seed_ =
    ( initSoundConfig, seed_ )
        |> (\( { duty, shape } as sc, seed ) ->
                let
                    ( ( newShape, newDuty ), newSeed ) =
                        Random.step
                            (Random.map2
                                (\shapeVal dutyVal ->
                                    if shapeVal == Noise then
                                        ( shapeVal, duty )

                                    else if shapeVal == Square then
                                        ( shapeVal, { duty | duty = dutyVal } )

                                    else if shapeVal == Sawtooth then
                                        ( shapeVal, { duty | duty = 1 } )

                                    else
                                        ( shapeVal, duty )
                                )
                                (chooseRandomShape [ Noise, Square, Sawtooth ])
                                (getFloat 0.6)
                            )
                            seed
                in
                ( { sc
                    | duty = newDuty
                    , shape = newShape
                  }
                , newSeed
                )
           )
        |> (\( { frequency } as sc, seed ) ->
                let
                    ( newFrequency, newSeed ) =
                        Random.step
                            (Random.map2
                                (\base ramp ->
                                    sc.frequency
                                        |> setFreqBase (0.2 + base)
                                        |> setFreqRamp (-0.3 - ramp)
                                )
                                (getFloat 0.6)
                                (getFloat 0.4)
                            )
                            seed
                in
                ( { sc | frequency = newFrequency }
                , newSeed
                )
           )
        |> (\( sc, seed ) ->
                let
                    ( newEnvelope, newSeed ) =
                        Random.step
                            (Random.map2
                                (\sustain decay ->
                                    sc.envelope
                                        |> setEnvAttack 0
                                        |> setEnvSustain sustain
                                        |> setEnvDecay (0.1 + decay)
                                )
                                (getFloat 0.1)
                                (getFloat 0.2)
                            )
                            seed
                in
                ( { sc | envelope = newEnvelope }
                , newSeed
                )
           )
        |> (\( { highPassFilter } as sc, seed ) ->
                let
                    ( newHighPassFilter, newSeed ) =
                        Random.step
                            (Random.map2
                                (\shouldFilter frequency ->
                                    if shouldFilter then
                                        { highPassFilter
                                            | frequency = frequency
                                        }

                                    else
                                        highPassFilter
                                )
                                flipCoin
                                (getFloat 0.3)
                            )
                            seed
                in
                ( { sc | highPassFilter = newHighPassFilter }
                , newSeed
                )
           )
        |> --TODO wavetypes
           identity


getRandomCoinPickup : Random.Seed -> ( SoundConfig, Random.Seed )
getRandomCoinPickup seed_ =
    ( initSoundConfig, seed_ )
        |> (\( sc, seed ) ->
                let
                    ( newFrequency, newSeed ) =
                        Random.step
                            (Random.map
                                (\base ->
                                    sc.frequency
                                        |> setFreqBase (0.4 + base)
                                )
                                (getFloat 0.5)
                            )
                            seed
                in
                ( { sc | frequency = newFrequency }
                , newSeed
                )
           )
        |> (\( sc, seed ) ->
                let
                    ( newEnvelope, newSeed ) =
                        Random.step
                            (Random.map3
                                (\sustain decay punch ->
                                    sc.envelope
                                        |> setEnvAttack 0
                                        |> setEnvSustain sustain
                                        |> setEnvDecay (0.1 + decay)
                                        |> setEnvPunch (0.3 + punch)
                                )
                                (getFloat 0.1)
                                (getFloat 0.4)
                                (getFloat 0.3)
                            )
                            seed
                in
                ( { sc | envelope = newEnvelope }
                , newSeed
                )
           )
        |> (\( { arpeggiation } as sc, seed ) ->
                let
                    ( newArpeggiation, newSeed ) =
                        Random.step
                            (Random.map3
                                (\shouldTweakArp speed mod ->
                                    if not shouldTweakArp then
                                        arpeggiation

                                    else
                                        { arpeggiation
                                            | speed = 0.5 + speed
                                            , mod = 0.2 + mod
                                        }
                                )
                                flipCoin
                                (getFloat 0.2)
                                (getFloat 0.4)
                            )
                            seed
                in
                ( { sc | arpeggiation = newArpeggiation }
                , newSeed
                )
           )


getRandomUpgrade : Random.Seed -> ( SoundConfig, Random.Seed )
getRandomUpgrade seed_ =
    ( initSoundConfig, seed_ )
        --TODO: sawtooth duty
        |> (\( { shape, duty } as sc, seed ) ->
                let
                    ( ( newShape, newDuty ), newSeed ) =
                        Random.step
                            (Random.map2
                                (\shouldSawtooth dutyVal ->
                                    if shouldSawtooth then
                                        ( Sawtooth, setDutyDuty 1 sc.duty )

                                    else
                                        ( shape, sc.duty |> setDutyDuty dutyVal )
                                )
                                flipCoin
                                (getFloat 0.6)
                            )
                            seed
                in
                ( { sc | duty = newDuty, shape = newShape }, newSeed )
           )
        |> (\( sc, seed ) ->
                let
                    ( newFrequency, newSeed ) =
                        Random.step
                            (Random.map2
                                (\base ramp ->
                                    sc.frequency
                                        |> setFreqBase (0.2 + base)
                                        |> setFreqRamp (0.1 + ramp)
                                )
                                (getFloat 0.3)
                                (getFloat 0.4)
                            )
                            seed
                in
                ( { sc | frequency = newFrequency }
                , newSeed
                )
           )
        --TODO else case handling p_freq_ramp, p_vib_strength and, p_vib_speed
        |> (\( sc, seed ) ->
                let
                    ( newRetrigger, newSeed ) =
                        Random.step
                            (Random.map
                                (\repeatSpeed ->
                                    sc.retrigger
                                        |> setRetriggerRepeatSpeed (0.4 + repeatSpeed)
                                )
                                (getFloat 0.4)
                            )
                            seed
                in
                ( { sc | retrigger = newRetrigger }, newSeed )
           )
        |> (\( sc, seed ) ->
                let
                    ( newEnvelope, newSeed ) =
                        Random.step
                            (Random.map2
                                (\sustain decay ->
                                    sc.envelope
                                        |> setEnvAttack 0
                                        |> setEnvSustain sustain
                                        |> setEnvDecay (0.1 + decay)
                                )
                                (getFloat 0.4)
                                (getFloat 0.4)
                            )
                            seed
                in
                ( { sc | envelope = newEnvelope }
                , newSeed
                )
           )


withShape : SoundConfig -> Shape -> SoundConfig
withShape soundConfig newShape =
    { soundConfig | shape = newShape }


updateShapeConfigType : Model -> ShapeUpdateType -> ( Model, Cmd Msg )
updateShapeConfigType ({ soundConfig } as model) (ShapeUpdateType shape) =
    shape
        |> withShape soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withFrequency : SoundConfig -> (Frequency -> Frequency) -> SoundConfig
withFrequency soundConfig updater =
    { soundConfig | frequency = updater soundConfig.frequency }


withVibrato : SoundConfig -> (Vibrato -> Vibrato) -> SoundConfig
withVibrato soundConfig updater =
    { soundConfig | vibrato = updater soundConfig.vibrato }


withSoundConfig : Model -> (SoundConfig -> SoundConfig) -> Model
withSoundConfig model updater =
    { model | soundConfig = updater model.soundConfig }


setSoundConfig : Model -> SoundConfig -> Model
setSoundConfig model newSoundConfig =
    { model | soundConfig = newSoundConfig }


withEnvelope : SoundConfig -> (Envelope -> Envelope) -> SoundConfig
withEnvelope soundConfig updater =
    { soundConfig | envelope = updater soundConfig.envelope }


updateEnvelopeConfigType : Model -> EnvelopeUpdateType -> ( Model, Cmd Msg )
updateEnvelopeConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        EnvAttack attack ->
            \env -> { env | attack = attack }

        EnvSustain sustain ->
            \env -> { env | sustain = sustain }

        EnvPunch punch ->
            \env -> { env | punch = punch }

        EnvDecay decay ->
            \env -> { env | decay = decay }
    )
        |> withEnvelope soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


updateFrequencyConfigType : Model -> FrequencyUpdateType -> ( Model, Cmd Msg )
updateFrequencyConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        FrqBase base ->
            \frq -> { frq | base = base }

        FrqLimit limit ->
            \frq -> { frq | limit = limit }

        FrqRamp ramp ->
            \frq -> { frq | ramp = ramp }

        FrqDramp dramp ->
            \frq -> { frq | dramp = dramp }
    )
        |> withFrequency soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


updateVibratoConfigType : Model -> VibratoUpdateType -> ( Model, Cmd Msg )
updateVibratoConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        VibStrength strength ->
            \vib -> { vib | strength = strength }

        VibSpeed speed ->
            \vib -> { vib | speed = speed }
    )
        |> withVibrato soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withArpeggiation : SoundConfig -> (Arpeggiation -> Arpeggiation) -> SoundConfig
withArpeggiation soundConfig updater =
    { soundConfig | arpeggiation = updater soundConfig.arpeggiation }


updateArpeggiationConfigType : Model -> ArpeggiationUpdateType -> ( Model, Cmd Msg )
updateArpeggiationConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        ArpMod mod ->
            \arp -> { arp | mod = mod }

        ArpSpeed speed ->
            \arp -> { arp | speed = speed }
    )
        |> withArpeggiation soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withDuty : SoundConfig -> (Duty -> Duty) -> SoundConfig
withDuty soundConfig updater =
    { soundConfig | duty = updater soundConfig.duty }


updateDutyConfigType : Model -> DutyUpdateType -> ( Model, Cmd Msg )
updateDutyConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        DtyDuty duty ->
            \dty -> { dty | duty = duty }

        DtyRamp ramp ->
            \dty -> { dty | ramp = ramp }
    )
        |> withDuty soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withRetrigger : SoundConfig -> (Retrigger -> Retrigger) -> SoundConfig
withRetrigger soundConfig updater =
    { soundConfig | retrigger = updater soundConfig.retrigger }


updateRetriggerConfigType : Model -> RetriggerUpdateType -> ( Model, Cmd Msg )
updateRetriggerConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        RetRepeatSpeed repeatSpeed ->
            \ret -> { ret | repeatSpeed = repeatSpeed }
    )
        |> withRetrigger soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withFlanger : SoundConfig -> (Flanger -> Flanger) -> SoundConfig
withFlanger soundConfig updater =
    { soundConfig | flanger = updater soundConfig.flanger }


updateFlangerConfigType : Model -> FlangerUpdateType -> ( Model, Cmd Msg )
updateFlangerConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        FlaOffset offset ->
            \fla -> { fla | offset = offset }

        FlaRamp ramp ->
            \fla -> { fla | ramp = ramp }
    )
        |> withFlanger soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withLowPassFilter : SoundConfig -> (LowPassFilter -> LowPassFilter) -> SoundConfig
withLowPassFilter soundConfig updater =
    { soundConfig | lowPassFilter = updater soundConfig.lowPassFilter }


updateLowPassFilterConfigType : Model -> LowPassFilterUpdateType -> ( Model, Cmd Msg )
updateLowPassFilterConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        LpfFrequency frequency ->
            \lpf -> { lpf | frequency = frequency }

        LpfRamp ramp ->
            \lpf -> { lpf | ramp = ramp }

        LpfResonance resonance ->
            \lpf -> { lpf | resonance = resonance }
    )
        |> withLowPassFilter soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withHighPassFilter : SoundConfig -> (HighPassFilter -> HighPassFilter) -> SoundConfig
withHighPassFilter soundConfig updater =
    { soundConfig | highPassFilter = updater soundConfig.highPassFilter }


updateHighPassFilterConfigType : Model -> HighPassFilterUpdateType -> ( Model, Cmd Msg )
updateHighPassFilterConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        HpfFrequency frequency ->
            \hpf -> { hpf | frequency = frequency }

        HpfRamp ramp ->
            \hpf -> { hpf | ramp = ramp }
    )
        |> withHighPassFilter soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


withMisc : SoundConfig -> (Misc -> Misc) -> SoundConfig
withMisc soundConfig updater =
    { soundConfig | misc = updater soundConfig.misc }


updateMiscConfigType : Model -> MiscUpdateType -> ( Model, Cmd Msg )
updateMiscConfigType ({ soundConfig } as model) updateType =
    (case updateType of
        MscVolume volume ->
            \msc -> { msc | volume = volume }

        MscSampleRate sampleRate ->
            \msc -> { msc | sampleRate = sampleRate }

        MscSampleSize sampleSize ->
            \msc -> { msc | sampleSize = sampleSize }
    )
        |> withMisc soundConfig
        |> setSoundConfig model
        |> (\m -> ( m, Cmd.none ))


updateOnSliderChanged : Model -> ConfigType -> ( Model, Cmd Msg )
updateOnSliderChanged model configType =
    let
        noop =
            ( model, Cmd.none )
    in
    case configType of
        ShapeConfigType updateType ->
            updateShapeConfigType model updateType

        EnvelopeConfigType updateType ->
            updateEnvelopeConfigType model updateType

        FrequencyConfigType updateType ->
            updateFrequencyConfigType model updateType

        VibratoConfigType updateType ->
            updateVibratoConfigType model updateType

        ArpeggiationConfigType updateType ->
            updateArpeggiationConfigType model updateType

        DutyConfigType updateType ->
            updateDutyConfigType model updateType

        RetriggerConfigType updateType ->
            updateRetriggerConfigType model updateType

        FlangerConfigType updateType ->
            updateFlangerConfigType model updateType

        LowPassFilterConfigType updateType ->
            updateLowPassFilterConfigType model updateType

        HighPassFilterConfigType updateType ->
            updateHighPassFilterConfigType model updateType

        MiscConfigType updateType ->
            updateMiscConfigType model updateType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        Noop ->
            noop

        SetHitHurt ->
            let
                ( newSoundConfig, newSeed ) =
                    getRandomHitHurt model.globalSeed
            in
            ( { model | globalSeed = newSeed, soundConfig = newSoundConfig }
            , sfxrOut <| encodeSoundConfig newSoundConfig
            )

        SetUpgrade ->
            let
                ( newSoundConfig, newSeed ) =
                    getRandomUpgrade model.globalSeed
            in
            ( { model | globalSeed = newSeed, soundConfig = newSoundConfig }
            , sfxrOut <| encodeSoundConfig newSoundConfig
            )

        SetCoinPickup ->
            let
                ( newSoundConfig, newSeed ) =
                    getRandomCoinPickup model.globalSeed
            in
            ( { model | globalSeed = newSeed, soundConfig = newSoundConfig }
            , sfxrOut <| encodeSoundConfig newSoundConfig
            )

        PlaySound ->
            ( model, sfxrOut <| encodeSoundConfig model.soundConfig )

        FromPort str ->
            let
                _ =
                    Debug.log "from port" str
            in
            noop

        OnSliderChanged configType ->
            updateOnSliderChanged model configType


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ --Time.every 1000 Tick,
          sfxrIn FromPort
        ]


viewShape : Shape -> Element Msg
viewShape shape =
    let
        shapeButton shapeType label =
            UI.button <|
                UI.TextParams
                    { buttonType =
                        if shapeType == shape then
                            UI.Secondary

                        else
                            UI.Outline
                    , colorTheme = UI.BrightTheme
                    , customAttrs = []
                    , onPressMsg =
                        shapeType
                            |> (ShapeUpdateType
                                    >> ShapeConfigType
                                    >> OnSliderChanged
                               )
                    , textLabel = label
                    }
    in
    row [ width fill, spacingXY 10 0 ]
        [ shapeButton Square "Square"
        , shapeButton Sawtooth "Sawtooth"
        , shapeButton Sine "Sine"
        , shapeButton Noise "Noise"
        ]


explain =
    Element.explain Debug.todo


paramSlider : List (Element.Attribute Msg) -> Bool -> (Float -> Msg) -> Float -> Element Msg
paramSlider attrs hideEmptySliders onChange value =
    let
        leftSize =
            round <| value * 1000

        rightSize =
            round <| (1 - value) * 1000

        background =
            row
                [ width fill
                , height <| Element.px 6
                , centerY
                ]
                [ el
                    [ Background.color <| UI.color_off_black
                    , width <| fillPortion leftSize
                    , height fill
                    , Border.width 1
                    , Border.rounded 2
                    ]
                  <|
                    Element.none
                , el
                    [ Background.color <| UI.color_grey
                    , width <| fillPortion rightSize
                    , height fill
                    , Border.width 1
                    , Border.rounded 2
                    ]
                  <|
                    Element.none
                ]
    in
    if hideEmptySliders && value == 0 then
        Element.none

    else
        Input.slider
            ([ width fill
             , Element.behindContent background
             , Element.mouseOver [ Element.scale 2.0, Background.color <| UI.color_primary ]
             ]
                ++ attrs
            )
            { onChange = onChange
            , label =
                Input.labelRight
                    [ Element.width (Element.px 100) ]
                <|
                    (text <| String.fromFloat value)
            , min = 0.0
            , max = 1.0
            , value = value
            , thumb =
                -- Input.defaultThumb
                Input.thumb
                    [ Element.mouseOver [ Element.scale 2.0, Background.color <| UI.color_primary ]
                    , height <| Element.px 20
                    , width <| Element.px 20
                    , Background.color UI.color_grey
                    , Border.rounded 10
                    , Border.width 2
                    , Border.color UI.color_off_black
                    ]
            , step = Just 0.00001
            }


viewEnvelope : Bool -> Envelope -> Element Msg
viewEnvelope hideEmptySliders envelope =
    let
        onChange =
            OnSliderChanged << EnvelopeConfigType
    in
    column [ width fill ]
        [ text "Envelope"
        , paramSlider [] hideEmptySliders (EnvAttack >> onChange) envelope.attack
        , paramSlider [] hideEmptySliders (EnvSustain >> onChange) envelope.sustain
        , paramSlider [] hideEmptySliders (EnvPunch >> onChange) envelope.punch
        , paramSlider [] hideEmptySliders (EnvDecay >> onChange) envelope.decay
        ]


viewFrequency : Bool -> Frequency -> Element Msg
viewFrequency hideEmptySliders frequency =
    let
        onChange =
            OnSliderChanged << FrequencyConfigType
    in
    column [ width fill ]
        [ text "Frequency"
        , paramSlider [] hideEmptySliders (FrqBase >> onChange) frequency.base
        , paramSlider [] hideEmptySliders (FrqLimit >> onChange) frequency.limit
        , paramSlider [] hideEmptySliders (FrqRamp >> onChange) frequency.ramp
        , paramSlider [] hideEmptySliders (FrqDramp >> onChange) frequency.dramp
        ]


viewVibrato : Bool -> Vibrato -> Element Msg
viewVibrato hideEmptySliders vibrato =
    let
        onChange =
            OnSliderChanged << VibratoConfigType
    in
    column [ width fill ]
        [ text "Vibrato"
        , paramSlider [] hideEmptySliders (VibStrength >> onChange) vibrato.strength
        , paramSlider [] hideEmptySliders (VibSpeed >> onChange) vibrato.speed
        ]


viewArpeggiation : Bool -> Arpeggiation -> Element Msg
viewArpeggiation hideEmptySliders arpeggiation =
    let
        onChange =
            OnSliderChanged << ArpeggiationConfigType
    in
    column [ width fill ]
        [ text "Arpeggiation"
        , paramSlider [] hideEmptySliders (ArpMod >> onChange) arpeggiation.mod
        , paramSlider [] hideEmptySliders (ArpSpeed >> onChange) arpeggiation.speed
        ]


viewDuty : Bool -> Duty -> Element Msg
viewDuty hideEmptySliders duty =
    let
        onChange =
            OnSliderChanged << DutyConfigType
    in
    column [ width fill ]
        [ text "Duty"
        , paramSlider [] hideEmptySliders (DtyDuty >> onChange) duty.duty
        , paramSlider [] hideEmptySliders (DtyRamp >> onChange) duty.ramp
        ]


viewRetrigger : Bool -> Retrigger -> Element Msg
viewRetrigger hideEmptySliders retrigger =
    let
        onChange =
            OnSliderChanged << RetriggerConfigType
    in
    column [ width fill ]
        [ text "Retrigger"
        , paramSlider [] hideEmptySliders (RetRepeatSpeed >> onChange) retrigger.repeatSpeed
        ]


viewFlanger : Bool -> Flanger -> Element Msg
viewFlanger hideEmptySliders flanger =
    let
        onChange =
            OnSliderChanged << FlangerConfigType
    in
    column [ width fill ]
        [ text "Flanger"
        , paramSlider [] hideEmptySliders (FlaOffset >> onChange) flanger.offset
        , paramSlider [] hideEmptySliders (FlaRamp >> onChange) flanger.ramp
        ]


viewLowPassFilter : Bool -> LowPassFilter -> Element Msg
viewLowPassFilter hideEmptySliders lowPassFilter =
    let
        onChange =
            OnSliderChanged << LowPassFilterConfigType
    in
    column [ width fill ]
        [ text "LowPassFilter"
        , paramSlider [] hideEmptySliders (LpfFrequency >> onChange) lowPassFilter.frequency
        , paramSlider [] hideEmptySliders (LpfRamp >> onChange) lowPassFilter.ramp
        , paramSlider [] hideEmptySliders (LpfResonance >> onChange) lowPassFilter.resonance
        ]


viewHighPassFilter : Bool -> HighPassFilter -> Element Msg
viewHighPassFilter hideEmptySliders highPassFilter =
    let
        onChange =
            OnSliderChanged << HighPassFilterConfigType
    in
    column [ width fill ]
        [ text "HighPassFilter"
        , paramSlider [] hideEmptySliders (HpfFrequency >> onChange) highPassFilter.frequency
        , paramSlider [] hideEmptySliders (HpfRamp >> onChange) highPassFilter.ramp
        ]


viewMisc : Bool -> Misc -> Element Msg
viewMisc hideEmptySliders misc =
    let
        onChange =
            OnSliderChanged << MiscConfigType
    in
    column [ width fill ]
        [ text "Misc"
        , paramSlider [] hideEmptySliders (MscVolume >> onChange) misc.volume
        , paramSlider [] hideEmptySliders (round >> MscSampleRate >> onChange) <| toFloat misc.sampleRate
        , paramSlider [] hideEmptySliders (round >> MscSampleSize >> onChange) <| toFloat misc.sampleSize
        ]


viewSliders : Model -> Element Msg
viewSliders ({ soundConfig, hideEmptySliders } as model) =
    column [ padding 10, width (fill |> Element.maximum 1000), spacing 10, centerX ]
        [ Lazy.lazy viewShape soundConfig.shape
        , Lazy.lazy2 viewEnvelope hideEmptySliders soundConfig.envelope
        , Lazy.lazy2 viewFrequency hideEmptySliders soundConfig.frequency
        , Lazy.lazy2 viewVibrato hideEmptySliders soundConfig.vibrato
        , Lazy.lazy2 viewArpeggiation hideEmptySliders soundConfig.arpeggiation
        , Lazy.lazy2 viewDuty hideEmptySliders soundConfig.duty
        , Lazy.lazy2 viewRetrigger hideEmptySliders soundConfig.retrigger
        , Lazy.lazy2 viewFlanger hideEmptySliders soundConfig.flanger
        , Lazy.lazy2 viewLowPassFilter hideEmptySliders soundConfig.lowPassFilter
        , Lazy.lazy2 viewHighPassFilter hideEmptySliders soundConfig.highPassFilter
        , Lazy.lazy2 viewMisc hideEmptySliders soundConfig.misc
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
        , Element.htmlAttribute <| Html.Attributes.id "sfxr"
        ]
    <|
        column [ width fill ]
            [ text "TEMP SFXR"
            , row [ width fill, spacing 10, padding 10 ]
                (let
                    button msg str =
                        UI.button <|
                            UI.TextParams
                                { buttonType = UI.Primary
                                , colorTheme = UI.BrightTheme
                                , customAttrs = []
                                , onPressMsg = msg
                                , textLabel = str
                                }
                 in
                 [ button PlaySound "Play"
                 , button SetHitHurt "RNG Hit/Hurt"
                 , button SetUpgrade "RNG Upgrade"
                 , button SetCoinPickup "RNG Coin/Pickup"
                 ]
                )
            , Lazy.lazy viewSliders model
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
