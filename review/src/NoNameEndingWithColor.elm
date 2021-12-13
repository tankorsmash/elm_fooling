module NoNameEndingWithColor exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-|

@docs rule

-}
stub =
    123


{-| Reports... REPLACEME

    config =
        [ NoNameEndingWithColor.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template undefined/example --rules NoNameEndingWithColor
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoNameEndingWithColor" ()
        -- Add your visitors
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        -- |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema



-- This function will visit all the expressions (like `1`, `"string"`, `foo bar`, `a + b`, ...)
-- and report problems that it finds


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            let
                functionName =
                    declaration |> Node.value |> .name

                functionNameStr : String
                functionNameStr =
                    declaration |> Node.value |> .name |> Node.value
            in
            if String.endsWith "_color" functionNameStr then
                -- Return a single error, describing the problem
                [ Rule.errorWithFix
                    { message = "Name should start with color_, not end with it"
                    , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                    }
                    -- This is the location of the problem in the source code
                    (Node.range (declaration |> Node.value |> .name))
                    [ Fix.replaceRangeBy (Node.range functionName) functionNameStr ]
                ]

            else
                []

        _ ->
            []



-- expressionVisitor : Node Expression -> List (Error {})
-- expressionVisitor node =
--     case Node.value node of
--         -- It will look at string literals (like "a", """a""")
--         Expression.FunctionOrValue mod_name str ->
--             -- let
--             --     _ =
--             --         if String.contains "color" str then
--             --             Debug.log "str" str
--             --
--             --         else
--             --             ""
--             -- in
--             if String.endsWith "_color" str then
--                 -- Return a single error, describing the problem
--                 [ Rule.errorWithFix
--                     { message = "Replace `XYX_color` by `color_XYZ`"
--                     , details = [ "Having names start with color_ makes it easier to search for" ]
--                     }
--                     -- This is the location of the problem in the source code
--                     (Node.range node)
--                     [ Fix.replaceRangeBy (Node.range node) str ]
--                 ]
--
--             else
--                 []
--
--         _ ->
--             []
