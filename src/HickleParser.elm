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


type LogicType
    = OrLogic
    | AndLogic



--


type Node
    = -- someVar = my_value
      ExpressionNode String String
    | -- OR, And
      LogicNode LogicType Node Node
    | StatementNode (List Node)


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


type alias NodeParser a =
    Parser.Parser Context Problem a


type alias NodeDeadEnd context problem =
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
    Parser.symbol
        (Parser.Token "("
            ExpectedOpenParen
        )


rightParen =
    Parser.symbol (Parser.Token ")" ExpectedClosedParen)


equalSign =
    Parser.symbol (Parser.Token "=" ExpectedEquals)


orOperator =
    Parser.keyword (Parser.Token "or" ExpectedOr)


andOperator =
    Parser.keyword (Parser.Token "and" ExpectedAnd)


inGenericContext str =
    Parser.inContext (GenericContext str)


expressionParser : List String -> NodeParser (List Node)
expressionParser _ =
    let
        expressions : NodeParser (List Node)
        expressions =
            inGenericContext "start" <|
                Parser.loop [] expressionsHelp

        -- finds ExpressionNode
        assignmentParser =
            inGenericContext "variableAssignment" <|
                Parser.succeed identity
                    |= Parser.oneOf
                        [ Parser.succeed ExpressionNode
                            |= getChompedAlpha
                            |. Parser.spaces
                            |. equalSign
                            |. Parser.spaces
                            |= getChompedAlpha
                        ]

        -- finds either ExpressionNode, OrNode or AndNode
        simpleNodeParser =
            inGenericContext "simple expression" <|
                (Parser.succeed identity
                    |= assignmentParser
                    |. Parser.spaces
                )

        expressionsHelp : List Node -> NodeParser (Parser.Step (List Node) (List Node))
        expressionsHelp foundSoFar =
            Parser.oneOf
                [ Parser.succeed (\expr -> Parser.Loop <| expr :: foundSoFar)
                    |. leftParen
                    |= simpleNodeParser
                    |. rightParen

                -- , Parser.succeed (\expr -> Parser.Loop <| expr :: foundSoFar)
                --     |= simpleNodeParser
                , Parser.succeed (\expr expr2 -> Parser.Loop <| LogicNode OrLogic expr expr2 :: foundSoFar)
                    |= simpleNodeParser
                    |. spaces
                    |. orOperator
                    |. spaces
                    |= simpleNodeParser
                , -- supposed to mark the end of the loop
                  inGenericContext "done" <|
                    Parser.succeed (Parser.Done foundSoFar)
                        |. Parser.end ExpectedEnd

                -- spaces
                , inGenericContext "spaces" <|
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


expectVariableNode : String -> String -> Node -> Expect.Expectation
expectVariableNode expectedLeft expectedRight expression =
    case expression of
        ExpressionNode left right ->
            Expect.all
                [ Tuple.first >> Expect.equal expectedLeft
                , Tuple.second >> Expect.equal expectedRight
                ]
                ( left, right )

        _ ->
            Expect.fail <| "expression is not a ExpressionNode: " ++ Debug.toString expression


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


expectParseSucceedsWithOneWithCondition : String -> (Node -> Expectation) -> Expectation
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


expectParseSucceedsWithMany : (List Node -> Expectation) -> Result (List (Parser.DeadEnd context problem)) (List Node) -> Expectation
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


expectParseSucceedsWithManyWithCondition : (List Node -> Expectation) -> Result (List (Parser.DeadEnd context problem)) (List Node) -> Expectation
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


expectFailToParse : Result (List (Parser.DeadEnd context problem)) (List Node) -> Expectation
expectFailToParse parseResult =
    case parseResult of
        Ok _ ->
            Expect.fail "Successfully parsed, didn't expect that"

        Err _ ->
            Expect.pass


suite : Test
suite =
    let
        expressionParseInput : String -> Result (List (Parser.DeadEnd Context Problem)) (List Node)
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
        , Test.only <|
            test "plain assignment" <|
                \_ ->
                    expectParseSucceedsWithOneWithCondition "username = Jackie"
                        (\expr ->
                            expectVariableNode "username" "Jackie" expr
                        )
        , Test.only <|
            test "assignment in parens" <|
                \_ ->
                    expectParseSucceedsWithOneWithCondition "(username = Jackie)"
                        (\expr ->
                            expectVariableNode "username" "Jackie" expr
                        )
        , Test.only <|
            test "plain or expression" <|
                \_ ->
                    expectParseSucceedsWithOneWithCondition "username = Jackie or country = canada"
                        (\expr ->
                            case expr of
                                LogicNode logicType left right ->
                                    case logicType of
                                        OrLogic ->
                                            Expect.all
                                                [ Tuple.first >> expectVariableNode "username" "Jackie"
                                                , Tuple.second >> expectVariableNode "country" "canada"
                                                ]
                                                ( left, right )

                                        _ ->
                                            Expect.fail "needs to be an or"

                                anythingElse ->
                                    Expect.fail <| "any other type of expression is a failure"
                        )
        , test "`someVar = a_value and someOtherVar = some_other_value` succeeds" <|
            \_ ->
                expectParseSucceedsWithOneWithCondition "username = Jackie and country = canada"
                    (\expr ->
                        case expr of
                            LogicNode logicType left right ->
                                case logicType of
                                    AndLogic ->
                                        Expect.all
                                            [ Tuple.first >> expectVariableNode "username" "Jackie"
                                            , Tuple.second >> expectVariableNode "country" "canada"
                                            ]
                                            ( left, right )

                                    _ ->
                                        Expect.fail "need and"

                            anythingElse ->
                                Expect.fail <| "any other type of expression is a failure"
                    )
        , test "Two assignments in parens around an AND" <|
            \_ ->
                expectParseSucceedsWithOneWithCondition "(username = Jackie) and (country = canada)"
                    (\expr ->
                        case expr of
                            LogicNode logicType left right ->
                                case logicType of
                                    AndLogic ->
                                        Expect.all
                                            [ Tuple.first >> expectVariableNode "username" "Jackie"
                                            , Tuple.second >> expectVariableNode "country" "canada"
                                            ]
                                            ( left, right )

                                    _ ->
                                        Expect.fail "ASDSAD"

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
