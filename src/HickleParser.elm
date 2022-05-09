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
import Parser exposing ((|.), (|=), chompIf, chompWhile, getChompedString)
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


type Expression
    = -- someVar = my_value
      VariableAssignment String String


expressionParser : List String -> Parser.Parser (List Expression)
expressionParser namesToFind =
    let
        expressions : Parser.Parser (List Expression)
        expressions =
            Parser.loop [] expressionsHelp

        expressionsHelp : List Expression -> Parser.Parser (Parser.Step (List Expression) (List Expression))
        expressionsHelp foundSoFar =
            Parser.oneOf
                [ -- parse a single word
                  Parser.succeed (\expr1 expr2 -> Parser.Loop (VariableAssignment expr1 expr2 :: foundSoFar))
                    |= (getChompedString <|
                            Parser.succeed ()
                                -- need to do an initial chompIf, because chompWhile succeeds even with 0 matches
                                |. Parser.chompIf Char.isAlphaNum
                                |. Parser.chompWhile Char.isLower
                       )
                    |. Parser.spaces
                    |. Parser.symbol "="
                    |. Parser.spaces
                    |= (getChompedString <|
                            Parser.succeed ()
                                -- need to do an initial chompIf, because chompWhile succeeds even with 0 matches
                                |. Parser.chompIf Char.isAlphaNum
                                |. Parser.chompWhile Char.isLower
                       )

                -- supposed to mark the end of the loop
                , Parser.succeed (Parser.Done foundSoFar)
                    |. Parser.end
                ]
    in
    expressions


suite : Test
suite =
    describe "Parser"
        [ test "`someVar = a_value` succeeds" <|
            \_ ->
                let
                    input =
                        "username = Jackie"

                    parseResult : Result (List Parser.DeadEnd) (List Expression)
                    parseResult =
                        Parser.run (expressionParser []) input
                in
                case parseResult of
                    Err err ->
                        Expect.fail "expected success"

                    Ok [] ->
                        Expect.fail "expected exactly one expression, found none"

                    Ok (expr :: rest) ->
                        case expr of
                            VariableAssignment left right ->
                                Expect.all
                                    [ \( l, r ) -> Expect.equal l "username"
                                    , \( l, r ) -> Expect.equal r "Jackie"
                                    ]
                                    ( left, right )
        ]
