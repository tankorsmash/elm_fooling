module HickleParser exposing (..)

import Array
import Console
import DateFormat.Relative
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra exposing (maybe)
import List.Extra
import Parser.Advanced as Parser exposing ((|.), (|=), chompIf, chompWhile, getChompedString, spaces)
import Process
import Random
import Random.List
import Task
import Test exposing (..)
import Time
import Tuple3


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
            |. Parser.chompIf Char.isAlphaNum ExpectedAlphaNum
            |. Parser.chompWhile Char.isLower


getChompedAlpha =
    getChompedString <|
        Parser.succeed ()
            -- need to do an initial chompIf, because chompWhile succeeds even with 0 matches
            |. Parser.chompIf Char.isAlpha ExpectedAlpha
            |. Parser.chompWhile Char.isLower


type alias ExpressionParser a =
    Parser.Parser Context Problem a


type alias ExpressionDeadEnd context problem =
    Parser.DeadEnd context problem


type Context
    = GenericContext String


type Problem
    = BadIndent
    | BadKeyword String
    | ExpectedEquals
    | ExpectedSpace
    | ExpectedAlphaNum
    | ExpectedAlpha
    | ExpectedOr
    | ExpectedAnd
    | ExpectedOpenParen
    | ExpectedClosedParen
    | ExpectedEnd


leftParen =
    Parser.symbol (Parser.Token "(" ExpectedOpenParen)


rightParen =
    Parser.symbol (Parser.Token ")" ExpectedClosedParen)


equalSign =
    Parser.symbol (Parser.Token "=" ExpectedEquals)


orOperator =
    Parser.keyword (Parser.Token "or" ExpectedOr)


andOperator =
    Parser.keyword (Parser.Token "and" ExpectedAnd)


expressionParser : List String -> ExpressionParser (List Expression)
expressionParser _ =
    let
        expressions : ExpressionParser (List Expression)
        expressions =
            Parser.inContext (GenericContext "start") <|
                Parser.loop [] expressionsHelp

        -- finds VariableAssignment
        assignmentParser =
            Parser.inContext (GenericContext "variableAssignment") <|
                Parser.succeed identity
                    |= Parser.oneOf
                        [ Parser.succeed VariableAssignment
                            |= getChompedAlpha
                            |. Parser.spaces
                            |. equalSign
                            |. Parser.spaces
                            |= getChompedAlpha
                        ]

        -- finds either VariableAssignment, OrExpression or AndExpression
        simpleExpressionParser =
            Parser.inContext (GenericContext "simple expression") <|
                (Parser.succeed identity
                    |= assignmentParser
                    |. Parser.spaces
                    |> Parser.andThen
                        (\expr ->
                            Parser.oneOf
                                [ -- either OrExpression continues
                                  Parser.inContext (GenericContext "trying to parse Or") <|
                                    Parser.succeed (\expr2 -> OrExpression expr expr2)
                                        |. orOperator
                                        |. Parser.spaces
                                        |= assignmentParser
                                , -- or the AndExpression continues
                                  Parser.inContext (GenericContext "trying to parse And") <|
                                    Parser.succeed (\expr2 -> AndExpression expr expr2)
                                        |. andOperator
                                        |. Parser.spaces
                                        |= assignmentParser
                                , -- or the VariableAssignment is over
                                  Parser.succeed
                                    expr
                                ]
                        )
                )

        expressionsHelp : List Expression -> ExpressionParser (Parser.Step (List Expression) (List Expression))
        expressionsHelp foundSoFar =
            Parser.oneOf
                [ Parser.inContext (GenericContext "open paren") <|
                    Parser.succeed (\expr -> expr)
                        |. leftParen
                        |= Parser.oneOf
                            [ -- either OrExpression continues
                              Parser.inContext (GenericContext "trying to parse Or") <|
                                Parser.succeed
                                    (\expr expr2 -> Parser.Loop <| OrExpression expr expr2 :: foundSoFar)
                                    |= assignmentParser
                                    |. orOperator
                                    |. Parser.spaces
                                    |= assignmentParser
                                    |. Parser.oneOf [ rightParen, Parser.succeed () ]
                            , -- or the AndExpression continues
                              Parser.inContext (GenericContext "trying to parse And") <|
                                Parser.succeed
                                    (\expr expr2 -> Parser.Loop <| AndExpression expr expr2 :: foundSoFar)
                                    |= assignmentParser
                                    |. orOperator
                                    |. Parser.spaces
                                    |= assignmentParser
                                    |. Parser.oneOf [ rightParen, Parser.succeed () ]
                            , Parser.succeed (Parser.Loop foundSoFar)
                                |. Parser.oneOf [ rightParen, Parser.succeed () ]
                            ]
                , Parser.inContext (GenericContext "no paren") <|
                    Parser.succeed (\expr -> expr)
                        |= Parser.oneOf
                            [ -- either OrExpression continues
                              Parser.inContext (GenericContext "trying to parse Or") <|
                                Parser.succeed
                                    (\expr expr2 -> Parser.Loop <| OrExpression expr expr2 :: foundSoFar)
                                    |= (Parser.inContext (GenericContext "left Or") <|
                                            assignmentParser
                                       )
                                    |. orOperator
                                    |. Parser.spaces
                                    |= (Parser.inContext (GenericContext "right Or") <|
                                            assignmentParser
                                       )
                            , -- or the AndExpression continues
                              Parser.inContext (GenericContext "trying to parse And") <|
                                Parser.succeed
                                    (\expr expr2 -> Parser.Loop <| AndExpression expr expr2 :: foundSoFar)
                                    |= (Parser.inContext (GenericContext "left And") <|
                                            assignmentParser
                                       )
                                    |. andOperator
                                    |. Parser.spaces
                                    |= (Parser.inContext (GenericContext "right And") <|
                                            assignmentParser
                                       )
                            ]

                -- supposed to mark the end of the loop
                , Parser.inContext (GenericContext "done") <|
                    Parser.succeed (Parser.Done foundSoFar)
                        |. Parser.end ExpectedEnd

                -- spaces
                , Parser.inContext (GenericContext "spaces") <|
                    Parser.succeed (Parser.Loop foundSoFar)
                        |. Parser.chompIf (\c -> c == ' ') ExpectedSpace
                        |. Parser.spaces
                ]
    in
    expressions


prettyPrintContext : String -> { row : Int, col : Int, context : Context } -> String
prettyPrintContext input { row, col, context } =
    let
        _ =
            Debug.log "context" ( context, col )

        _ =
            Debug.log "input" <|
                String.slice 0 (col - 1) input
                    ++ Console.green (String.dropLeft (col - 1) input)
    in
    ""


explainProblem : String -> Parser.DeadEnd Context problem -> String
explainProblem input ({ problem, row, col } as deadEnd) =
    let
        _ =
            Debug.log "-- prob,row,col" ( problem, row, col )

        _ =
            List.map (prettyPrintContext input) deadEnd.contextStack
    in
    "see debug"



-- case problem of
--     Parser.UnexpectedChar ->
--         "UnexpectedChar '"
--             ++ String.slice (col - 1) col input
--             ++ "' at "
--             ++ String.fromInt row
--             ++ "/"
--             ++ String.fromInt col
--             ++ " for: \""
--             ++ input
--             ++ "\""
--
--     unknownErr ->
--         "some unknown parsing error: '"
--             ++ Debug.toString unknownErr
--             ++ "' '"
--             ++ String.slice (col - 1) col input
--             ++ "' at "
--             ++ String.fromInt row
--             ++ "/"
--             ++ String.fromInt col
--             ++ " for: \""
--             ++ input
--             ++ "\""


explainProblems : String -> List (Parser.DeadEnd Context problem) -> String
explainProblems input deadEnds =
    deadEnds
        |> Debug.log "num dead ends"
        << List.length
        |> always deadEnds
        |> List.map (explainProblem input)
        |> String.join " -- "


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


expectParseSucceedsWithZero : String -> Expectation
expectParseSucceedsWithZero input =
    case Parser.run (expressionParser []) input of
        Ok success ->
            Expect.equal 0 (List.length success)

        Err deadEnds ->
            Expect.fail <|
                "Failed to parse, instead of successfully parsing an expression list of length 0"
                    ++ explainProblems input deadEnds


expectParseSucceedsWithOne : String -> Expectation
expectParseSucceedsWithOne input =
    case Parser.run (expressionParser []) input of
        Ok success ->
            Expect.equal 1 (List.length success)

        Err problems ->
            Expect.fail <|
                "Failed to parse, instead of successfully parsing an expression list of length 1"
                    ++ explainProblems input problems


expectParseSucceedsWithOneWithCondition : String -> (Expression -> Expectation) -> Expectation
expectParseSucceedsWithOneWithCondition input exprTester =
    case Parser.run (expressionParser []) input of
        Ok [] ->
            Expect.fail "expected only one expression, found 0"

        Ok (expr :: []) ->
            exprTester expr

        Ok any ->
            Expect.fail "expected only one expression, found many"

        Err problems ->
            Expect.fail <|
                "Failed to parse, instead of successfully parsing an expression list of length 1:"
                    ++ explainProblems input problems


expectParseSucceedsWithMany : (List Expression -> Expectation) -> Result (List (Parser.DeadEnd context problem)) (List Expression) -> Expectation
expectParseSucceedsWithMany exprTester parseResult =
    case parseResult of
        Ok [] ->
            Expect.fail "expected only one expression, found 0"

        Ok (expr :: []) ->
            Expect.fail "expected many expressions, found 1"

        Ok manyExprs ->
            Expect.pass

        Err _ ->
            Expect.fail "Failed to parse, instead of successfully parsing an expression list of many"


expectParseSucceedsWithManyWithCondition : (List Expression -> Expectation) -> Result (List (Parser.DeadEnd context problem)) (List Expression) -> Expectation
expectParseSucceedsWithManyWithCondition exprTester parseResult =
    case parseResult of
        Ok [] ->
            Expect.fail "expected only one expression, found 0"

        Ok (expr :: []) ->
            Expect.fail "expected many expressions, found 1"

        Ok manyExprs ->
            exprTester manyExprs

        Err _ ->
            Expect.fail "Failed to parse, Expected a parsed expression list of length 1"


expectFailToParse : Result (List (Parser.DeadEnd context problem)) (List Expression) -> Expectation
expectFailToParse parseResult =
    case parseResult of
        Ok _ ->
            Expect.fail "Successfully parsed, didn't expect that"

        Err _ ->
            Expect.pass


suite : Test
suite =
    let
        expressionParseInput : String -> Result (List (Parser.DeadEnd Context Problem)) (List Expression)
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
                expectParseSucceedsWithOne "A=A"
        , test "0=0 fails" <|
            \_ ->
                expectFailToParse <| expressionParseInput "0=0"
        , test "plain assignment" <|
            \_ ->
                expectParseSucceedsWithOneWithCondition "username = Jackie"
                    (\expr ->
                        expectVariableExpression "username" "Jackie" expr
                    )
        , test "assignment in parens" <|
            \_ ->
                expectParseSucceedsWithOneWithCondition "(username = Jackie)"
                    (\expr ->
                        expectVariableExpression "username" "Jackie" expr
                    )
        , Test.only <|
            test "plain or expression" <|
                \_ ->
                    expectParseSucceedsWithOneWithCondition "username = Jackie or country = canada"
                        (\expr ->
                            case expr of
                                OrExpression left right ->
                                    Expect.all
                                        [ Tuple.first >> expectVariableExpression "username" "Jackie"
                                        , Tuple.second >> expectVariableExpression "country" "canada"
                                        ]
                                        ( left, right )

                                anythingElse ->
                                    Expect.fail <| "any other type of expression is a failure"
                        )
        , test "`someVar = a_value and someOtherVar = some_other_value` succeeds" <|
            \_ ->
                expectParseSucceedsWithOneWithCondition "username = Jackie and country = canada"
                    (\expr ->
                        case expr of
                            AndExpression left right ->
                                Expect.all
                                    [ Tuple.first >> expectVariableExpression "username" "Jackie"
                                    , Tuple.second >> expectVariableExpression "country" "canada"
                                    ]
                                    ( left, right )

                            anythingElse ->
                                Expect.fail <| "any other type of expression is a failure"
                    )
        , test "Two assignments in parens around an AND" <|
            \_ ->
                expectParseSucceedsWithOneWithCondition "(username = Jackie) and (country = canada)"
                    (\expr ->
                        case expr of
                            AndExpression left right ->
                                Expect.all
                                    [ Tuple.first >> expectVariableExpression "username" "Jackie"
                                    , Tuple.second >> expectVariableExpression "country" "canada"
                                    ]
                                    ( left, right )

                            anythingElse ->
                                Expect.fail <| "any other type of expression is a failure"
                    )
        , test "empty string parses as empty list of expressions" <|
            \_ ->
                expectParseSucceedsWithZero ""
        , test "\"()\" string parses as empty list of expressions" <|
            \_ ->
                expectParseSucceedsWithZero "()"
        ]
