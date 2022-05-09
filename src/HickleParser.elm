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
    | -- OR expression
      OrExpression Expression Expression
    | -- And expression
      AndExpression Expression Expression


getChompedAlphaNum =
    getChompedString <|
        Parser.succeed ()
            -- need to do an initial chompIf, because chompWhile succeeds even with 0 matches
            |. Parser.chompIf Char.isAlphaNum
            |. Parser.chompWhile Char.isLower


getChompedAlpha =
    getChompedString <|
        Parser.succeed ()
            -- need to do an initial chompIf, because chompWhile succeeds even with 0 matches
            |. Parser.chompIf Char.isAlpha
            |. Parser.chompWhile Char.isLower


expressionParser : List String -> Parser.Parser (List Expression)
expressionParser namesToFind =
    let
        expressions : Parser.Parser (List Expression)
        expressions =
            Parser.loop [] expressionsHelp

        -- finds VariableAssignment
        variableAssignmentParser =
            Parser.succeed (\expr1 expr2 -> VariableAssignment expr1 expr2)
                |= getChompedAlpha
                |. Parser.spaces
                |. Parser.symbol "="
                |. Parser.spaces
                |= getChompedAlpha

        -- finds either VariableAssignment, OrExpression or AndExpression
        simpleExpressionParser =
            variableAssignmentParser
                |. Parser.spaces
                |> Parser.andThen
                    (\expr ->
                        Parser.oneOf
                            [ -- either OrExpression continues
                              Parser.succeed
                                (\expr2 -> OrExpression expr expr2)
                                |. Parser.keyword "or"
                                |. Parser.spaces
                                |= variableAssignmentParser
                            , -- or the AndExpression continues
                              Parser.succeed
                                (\expr2 -> AndExpression expr expr2)
                                |. Parser.keyword "and"
                                |. Parser.spaces
                                |= variableAssignmentParser
                            , -- or the VariableAssignment is over
                              Parser.succeed
                                expr
                            ]
                    )

        expressionsHelp : List Expression -> Parser.Parser (Parser.Step (List Expression) (List Expression))
        expressionsHelp foundSoFar =
            Parser.oneOf
                [ -- parse OrExpression or VariableAssignment
                  Parser.succeed (\expr -> Parser.Loop <| expr :: foundSoFar)
                    |= simpleExpressionParser
                , Parser.succeed Parser.Loop
                    |. Parser.symbol "("
                    |= Parser.oneOf
                        [ Parser.succeed (\expr -> expr :: foundSoFar)
                            |= simpleExpressionParser
                            |> Parser.andThen
                                (\expr ->
                                    Parser.succeed expr
                                        |. Parser.symbol ")"
                                )
                        , Parser.succeed foundSoFar
                            |. Parser.symbol ")"
                        ]

                -- supposed to mark the end of the loop
                , Parser.succeed (Parser.Done foundSoFar)
                    |. Parser.end
                ]
    in
    expressions


explainProblem : Parser.DeadEnd -> String -> String
explainProblem { problem, row, col } input =
    case problem of
        Parser.UnexpectedChar ->
            "UnexpectedChar '"
                ++ String.slice (col - 1) col input
                ++ "' at "
                ++ String.fromInt row
                ++ "/"
                ++ String.fromInt col
                ++ " for: \""
                ++ input
                ++ "\""

        unknownErr ->
            "some unknown parsing error: '" ++ Debug.toString unknownErr ++ "'"


expectVariableExpression : String -> String -> Expression -> Expect.Expectation
expectVariableExpression expectedLeft expectedRight expression =
    case expression of
        VariableAssignment left right ->
            Expect.all
                [ Tuple.first >> Expect.equal expectedLeft
                , Tuple.second >> Expect.equal expectedRight
                ]
                ( left, right )

        _ ->
            Expect.fail <| "expression is not a VariableAssignment: " ++ Debug.toString expression


expectParseSucceedsWithZero : Result (List Parser.DeadEnd) (List Expression) -> Expectation
expectParseSucceedsWithZero parseResult =
    case parseResult of
        Ok success ->
            Expect.equal 0 (List.length success)

        Err _ ->
            Expect.fail "Failed to parse, Expected a parsed expression list of length 0"


expectParseSucceedsWithOne : Result (List Parser.DeadEnd) (List Expression) -> Expectation
expectParseSucceedsWithOne parseResult =
    case parseResult of
        Ok success ->
            Expect.equal 1 (List.length success)

        Err _ ->
            Expect.fail "Failed to parse, Expected a parsed expression list of length 0"


expectFailToParse : Result (List Parser.DeadEnd) (List Expression) -> Expectation
expectFailToParse parseResult =
    case parseResult of
        Ok _ ->
            Expect.fail "Successfully parsed, didn't expect that"

        Err _ ->
            Expect.pass


suite : Test
suite =
    let
        expressionParseInput : String -> Result (List Parser.DeadEnd) (List Expression)
        expressionParseInput input =
            Parser.run (expressionParser []) input
    in
    describe "Parser"
        -- [ fuzz Fuzz.string "bogus fails" <|
        --     \input ->
        --         (\result ->
        --             case result of
        --                 Ok success ->
        --                     Expect.equal 0 (List.length success)
        --
        --                 Err _ ->
        --                     Expect.pass
        --         )
        --         <|
        --             Parser.run (expressionParser []) input
        [ test "A=A passes" <|
            \_ ->
                expectParseSucceedsWithOne <| expressionParseInput "A=A"
        , test "0=0 fails" <|
            \_ ->
                expectFailToParse <| expressionParseInput "0=0"
        , test "`someVar = a_value` succeeds" <|
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
                        Expect.fail <|
                            "expected parse success, but got: "
                                ++ (err
                                        |> List.map (\e -> explainProblem e input)
                                        |> String.join " -- "
                                   )

                    Ok [] ->
                        Expect.fail "expected exactly one expression, found none"

                    Ok (expr :: rest) ->
                        expectVariableExpression "username" "Jackie" expr
        , test "`(someVar = a_value)` succeeds" <|
            \_ ->
                let
                    input =
                        "(username = Jackie)"

                    parseResult : Result (List Parser.DeadEnd) (List Expression)
                    parseResult =
                        Parser.run (expressionParser []) input
                in
                case parseResult of
                    Err err ->
                        Expect.fail <|
                            "expected parse success, but got: "
                                ++ (err
                                        |> List.map (\e -> explainProblem e input)
                                        |> String.join " -- "
                                   )

                    Ok [] ->
                        Expect.fail "expected exactly one expression, found none"

                    Ok (expr :: rest) ->
                        expectVariableExpression "username" "Jackie" expr
        , test "`someVar = a_value or someOtherVar = some_other_value` succeeds" <|
            \_ ->
                let
                    input =
                        "username = Jackie or country = canada"

                    parseResult : Result (List Parser.DeadEnd) (List Expression)
                    parseResult =
                        Parser.run (expressionParser []) input
                in
                case parseResult of
                    Err err ->
                        Expect.fail <|
                            "expected parse success, but got: "
                                ++ (err
                                        |> List.map (\e -> explainProblem e input)
                                        |> String.join " -- "
                                   )

                    Ok [] ->
                        Expect.fail "expected exactly one expression, found none"

                    Ok (expr :: rest) ->
                        case expr of
                            OrExpression left right ->
                                Expect.all
                                    [ Tuple.first >> expectVariableExpression "username" "Jackie"
                                    , Tuple.second >> expectVariableExpression "country" "canada"
                                    ]
                                    ( left, right )

                            anythingElse ->
                                Expect.fail <| "any other type of expression is a failure"
        , test "`someVar = a_value and someOtherVar = some_other_value` succeeds" <|
            \_ ->
                let
                    input =
                        "username = Jackie and country = canada"

                    parseResult : Result (List Parser.DeadEnd) (List Expression)
                    parseResult =
                        Parser.run (expressionParser []) input
                in
                case parseResult of
                    Err err ->
                        Expect.fail <|
                            "expected parse success, but got: "
                                ++ (err
                                        |> List.map (\e -> explainProblem e input)
                                        |> String.join " -- "
                                   )

                    Ok [] ->
                        Expect.fail "expected exactly one expression, found none"

                    Ok (expr :: rest) ->
                        case expr of
                            AndExpression left right ->
                                Expect.all
                                    [ Tuple.first >> expectVariableExpression "username" "Jackie"
                                    , Tuple.second >> expectVariableExpression "country" "canada"
                                    ]
                                    ( left, right )

                            anythingElse ->
                                Expect.fail <| "any other type of expression is a failure"
        , test "empty string parses as empty list of expressions" <|
            \_ ->
                let
                    input =
                        ""

                    parseResult : Result (List Parser.DeadEnd) (List Expression)
                    parseResult =
                        Parser.run (expressionParser []) input
                in
                parseResult
                    |> Result.map
                        (\ok -> Expect.true "No parsed expressions" <| List.isEmpty ok)
                    |> Result.withDefault
                        (Expect.fail "blank string failed to parse")
        , test "\"()\" string parses as empty list of expressions" <|
            \_ ->
                let
                    input =
                        "()"

                    parseResult : Result (List Parser.DeadEnd) (List Expression)
                    parseResult =
                        Parser.run (expressionParser []) input
                in
                parseResult
                    |> Result.map
                        (\ok -> Expect.true "No parsed expressions" <| List.isEmpty ok)
                    |> Result.withDefault
                        (Expect.fail "pair of parens failed to parse")
        ]
