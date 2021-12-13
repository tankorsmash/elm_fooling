module NoNameEndingWithColor exposing (rule)

import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Direction, Error, Rule)


{-|

@docs rule

-}
stub =
    123


{-| dict of func names we're replacing
-}
type alias AllFixesToApply =
    Dict.Dict String Fixes


type alias FixesToApply =
    ( String, Fixes )


type alias Fixes =
    List ( Range, List Fix.Fix )



-- type AllFixesToApply
--     = All
--     | OnlySome (List String)


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
    Rule.newModuleRuleSchema "NoNameEndingWithColor" Dict.empty
        -- Add your visitors
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


fix_color_str str =
    if not <| String.endsWith "_color" str then
        str

    else
        str
            |> String.replace "_color" ""
            |> String.append "color_"


validate_exposing_node : Node Exposing.TopLevelExpose -> Maybe FixesToApply
validate_exposing_node exposed_val =
    case Node.value exposed_val of
        Exposing.FunctionExpose functionNameStr ->
            let
                node_range =
                    Node.range exposed_val
            in
            if String.endsWith "_color" functionNameStr then
                -- Return a single error, describing the problem
                -- [ Rule.errorWithFix
                --     { message = "Exposed names should start with color_, not end with it"
                --     , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                --     }
                --     -- This is the location of the problem in the source code
                --     node_range
                --     [ Fix.replaceRangeBy node_range (fix_color_str functionNameStr) ]
                -- ]
                Just ( functionNameStr, [ ( node_range, [ Fix.replaceRangeBy node_range (fix_color_str functionNameStr) ] ) ] )

            else
                Nothing

        Exposing.InfixExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose _ ->
            Nothing

        Exposing.TypeExpose _ ->
            Nothing


declarationVisitor : Node Declaration -> Direction -> AllFixesToApply -> ( List (Error {}), AllFixesToApply )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.FunctionDeclaration { signature, documentation, declaration } ) ->
            let
                signatureError : Fixes
                signatureError =
                    case signature of
                        Just sig_node ->
                            sig_node
                                |> Node.value
                                |> .name
                                |> (\sig_name_node ->
                                        let
                                            sig_name_range =
                                                Node.range sig_name_node
                                        in
                                        [ ( sig_name_range, [ Fix.replaceRangeBy sig_name_range (fix_color_str functionNameStr) ] ) ]
                                   )

                        Nothing ->
                            []

                functionName =
                    declaration |> Node.value |> .name

                functionNameStr : String
                functionNameStr =
                    declaration |> Node.value |> .name |> Node.value

                node_range =
                    Node.range functionName
            in
            if String.endsWith "_color" functionNameStr then
                -- ( [ Rule.errorWithFix
                --         { message = "Function names should start with color_, not end with it"
                --         , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                --         }
                --         -- This is the location of the problem in the source code
                --         node_range
                --         [ Fix.replaceRangeBy node_range (fix_color_str functionNameStr) ]
                --   ]
                --     ++ signatureError
                -- , context
                -- )
                ( []
                , Dict.update
                    functionNameStr
                    (\mb_existing_fixes ->
                        let
                            new_entry =
                                [ ( node_range, [ Fix.replaceRangeBy node_range (fix_color_str functionNameStr) ] ) ] ++ signatureError
                        in
                        case mb_existing_fixes of
                            Nothing ->
                                Just new_entry

                            Just existing_fixes ->
                                Just <| List.append existing_fixes new_entry
                    )
                    context
                )

            else
                ( [], context )

        _ ->
            ( [], context )


moduleDefinitionVisitor : Node Module -> AllFixesToApply -> ( List (Error {}), AllFixesToApply )
moduleDefinitionVisitor node context =
    case Node.value node |> Module.exposingList of
        Exposing.All _ ->
            ( [], context )

        Exposing.Explicit exposedValues ->
            ( []
            , let
                fixes_to_apply : List ( String, List ( Range, List Fix.Fix ) )
                fixes_to_apply =
                    List.filterMap
                        validate_exposing_node
                        exposedValues
              in
              List.foldl
                (\( name, fixes ) c ->
                    Dict.update
                        name
                        (\m ->
                            case m of
                                Nothing ->
                                    Just fixes

                                Just existing_fixes ->
                                    Just <| List.append existing_fixes fixes
                        )
                        c
                )
                context
                fixes_to_apply
              -- fixes_to_apply
              --   |> List.concatMap identity
              --   |> List.reverse
              --   |> List.append context
            )


fixes_to_apply_to_error : FixesToApply -> List (Error {})
fixes_to_apply_to_error ( functionNameStr, range_fixes ) =
    case range_fixes of
        ( first_range, first_fixes ) :: rest_range_and_fixes ->
            [ Rule.errorWithFix
                { message = "Exposed names should start with color_, not end with it"
                , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                }
                -- This is the location of the problem in the source code
                first_range
                (first_fixes ++ (List.concatMap identity <| List.map Tuple.second rest_range_and_fixes))
            ]

        [] ->
            []


finalEvaluation : AllFixesToApply -> List (Error {})
finalEvaluation context =
    context
        |> Dict.toList
        |> List.map
            fixes_to_apply_to_error
        |> List.concatMap identity



-- case ( Dict.get [ "Element" ] context, Dict.get [ "Html", "Styled" ] context ) of
--     ( Just elmUiRange, Just _ ) ->
--         [ Rule.error
--             { message = "Do not use both `elm-ui` and `elm-css`"
--             , details = [ "At fruits.com, we use `elm-ui` in the dashboard application, and `elm-css` in the rest of the code. We want to use `elm-ui` in our new projects, but in projects using `elm-css`, we don't want to use both libraries to keep things simple." ]
--             }
--             elmUiRange
--         ]
--     _ ->
--         []
