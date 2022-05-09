module HickleParser exposing (..)

import Animator
import Array
import Battle
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Color
import Color.Convert as Convert
import DOM exposing (offsetWidth, target)
import DateFormat.Relative
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
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Html
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events
import Interface as UI exposing (ColorTheme(..), defaultRounded)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra exposing (maybe)
import List.Extra
import Parser exposing ((|.), (|=))
import Process
import Random
import Random.List
import Sfxr
import Task
import Test exposing (..)
import Time
import Tuple3
import UUID exposing (UUID)
import Url


type Name
    = Name String


namesParser : List String -> Parser.Parser (List Name)
namesParser namesToFind =
    let
        names : Parser.Parser (List Name)
        names =
            Parser.loop [] namesHelp

        namesHelp : List Name -> Parser.Parser (Parser.Step (List Name) (List Name))
        namesHelp revNames =
            Parser.oneOf
                -- find and upper case word, and keep it if it matches
                [ Parser.succeed
                    (Maybe.map (\n -> n :: revNames)
                        >> Maybe.withDefault revNames
                        >> Parser.Loop
                    )
                    |= parseName

                -- parse a single space
                , Parser.succeed (Parser.Loop revNames)
                    |. Parser.chompIf (\c -> c == ' ')

                -- parse a non alpha character
                , Parser.succeed (Parser.Loop revNames)
                    |. Parser.chompIf (not << Char.isAlphaNum)

                -- parse a single word
                , Parser.succeed (Parser.Loop revNames)
                    -- need to do an initial chompIf, because chompWhile succeeds even with 0 matches
                    |. Parser.chompIf Char.isAlphaNum
                    |. Parser.chompWhile Char.isLower

                -- supposed to mark the end of the loop
                , Parser.succeed (Parser.Done (List.reverse revNames))
                    |. Parser.end
                ]

        parseName : Parser.Parser (Maybe Name)
        parseName =
            (Parser.getChompedString <|
                Parser.succeed identity
                    |. Parser.chompIf Char.isUpper
                    |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')
            )
                |> Parser.andThen
                    (\str ->
                        if List.member str namesToFind then
                            Parser.succeed (Just <| Name str)

                        else
                            -- Parser.problem ("Invalid name: " ++ str)
                            Parser.succeed Nothing
                    )
    in
    names


suite : Test
suite =
    let
        _ =
            123
    in
    describe "Parser tests"
        [ test "fake test" <|
            \_ ->
                Expect.pass
        ]
