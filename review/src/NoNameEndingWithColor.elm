module NoNameEndingWithColor exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Direction, Error, Rule)


{-|

@docs rule

-}
stub =
    123


type ExposedFunctions
    = All
    | OnlySome (List String)


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
    -- Rule.newModuleRuleSchema "NoNameEndingWithColor" ()
    Rule.newModuleRuleSchema "NoNameEndingWithColor" (OnlySome [])
        -- Add your visitors
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        -- |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        -- |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


fix_color_str str =
    if not <| String.endsWith "_color" str then
        str

    else
        str
            |> String.replace "_color" ""
            |> String.append "color_"


validate_exposing_node : Node Exposing.TopLevelExpose -> List (Error {})
validate_exposing_node exposed_val =
    case Node.value exposed_val of
        Exposing.FunctionExpose functionNameStr ->
            let
                node_range =
                    Node.range exposed_val
            in
            if String.endsWith "_color" functionNameStr then
                -- Return a single error, describing the problem
                [ Rule.errorWithFix
                    { message = "Name should start with color_, not end with it"
                    , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                    }
                    -- This is the location of the problem in the source code
                    node_range
                    [ Fix.replaceRangeBy node_range (fix_color_str functionNameStr) ]
                ]

            else
                []

        Exposing.InfixExpose _ ->
            []

        Exposing.TypeOrAliasExpose _ ->
            []

        Exposing.TypeExpose _ ->
            []



declarationVisitor : Node Declaration -> Direction -> ExposedFunctions -> ( List (Error {}), ExposedFunctions )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.FunctionDeclaration { documentation, declaration } ) ->
            let
                functionName =
                    declaration |> Node.value |> .name

                functionNameStr : String
                functionNameStr =
                    declaration |> Node.value |> .name |> Node.value

                node_range =
                    Node.range functionName

            in
            if String.endsWith "_color" functionNameStr then
                ( [ Rule.errorWithFix
                        { message = "Name should start with color_, not end with it"
                        , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                        }
                        -- This is the location of the problem in the source code
                        node_range
                        [ Fix.replaceRangeBy node_range (fix_color_str functionNameStr) ]
                  ]
                , context
                )

            else
                ( [], context )
        _ ->
            ( [], context )


isExposed : ExposedFunctions -> String -> Bool
isExposed exposedFunctions name =
    case exposedFunctions of
        All ->
            True

        OnlySome exposedList ->
            List.member name exposedList


moduleDefinitionVisitor : Node Module -> ExposedFunctions -> ( List (Error {}), ExposedFunctions )
moduleDefinitionVisitor node context =
    case Node.value node |> Module.exposingList of
        Exposing.All _ ->
            ( [], All )

        Exposing.Explicit exposedValues ->
            ( let
                _ =
                    Debug.log "exposedValues" exposedValues

                color_errors =
                    List.map
                        validate_exposing_node
                        exposedValues

                _ = Debug.log "num color_errors" <| List.length color_errors
              in
              List.concatMap identity color_errors
            , OnlySome (List.filterMap exposedFunctionName exposedValues)
            )


exposedFunctionName : Node Exposing.TopLevelExpose -> Maybe String
exposedFunctionName value =
    case Node.value value of
        Exposing.FunctionExpose functionName ->
            Just functionName

        _ ->
            Nothing

