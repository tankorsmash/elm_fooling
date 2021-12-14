module NoNameEndingWithColor exposing (rule)

import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature as Signature exposing (Signature)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Direction, Error, Rule)


{-|

@docs Disallows variables ending with `_color`

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


{-| Reports names ending with `_color` which should instead begin with `color_`.

    config =
        [ NoNameEndingWithColor.rule
        ]


## Fail

    red_color : Int
    red_color =
        123


## Success

    color_red : Int
    color_red =
        123


## When (not) to enable this rule

This rule is useful when you've got several color helpers
This rule is not useful when you've got several functions defined with \_colors at the end of the name.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template undefined/example --rules NoNameEndingWithColor
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoNameEndingWithColor" Dict.empty
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


is_invalid_color_str : String -> Bool
is_invalid_color_str str =
    String.endsWith "_color" str


fix_color_str : String -> String
fix_color_str str =
    if not <| is_invalid_color_str str then
        str

    else
        str
            |> String.replace "_color" ""
            |> String.append "color_"


expressionExitVisitor : Node Expression -> AllFixesToApply -> ( List (Error {}), AllFixesToApply )
expressionExitVisitor node context =
    -- ( [], context)
    case Node.value node of
        -- When exiting the let expression, report the variables that were not used.
        Expression.LetExpression { declarations, expression } ->
            let
                fixes_to_apply =
                    declarations
                        |> List.map
                            (\ld ->
                                case Node.value ld of
                                    Expression.LetFunction func ->
                                        Just <|
                                            let
                                                ( fname, decl_fixes ) =
                                                    validate_declaration_with_fix
                                                        func.declaration

                                                sig_fixes =
                                                    validate_signature func.signature
                                            in
                                            ( fname, decl_fixes ++ sig_fixes )

                                    Expression.LetDestructuring _ _ ->
                                        Nothing
                            )
                        |> List.filterMap identity
            in
            ( []
            , List.foldl
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
            )

        --     ( unusedVariables context |> List.map createError, removeVariables context )
        -- Expression.Function func ->
        --     ( [], context )
        -- Expression.FunctionImplementation { name, arguments, expression } ->
        Expression.FunctionOrValue moduleName string ->
            if is_invalid_color_str string then
                let
                    node_range =
                        Node.range node
                in
                ( []
                , Dict.update
                    string
                    (\mb_existing_fixes ->
                        let
                            new_entry =
                                [ build_fix_value node string ]
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


build_fix_value node color_str =
    let
        node_range =
            Node.range node
    in
    ( node_range
    , [ Fix.replaceRangeBy node_range
            (fix_color_str color_str)
      ]
    )


build_fix node color_str =
    ( color_str, [ build_fix_value node color_str ] )


validate_exposing_node : Node Exposing.TopLevelExpose -> Maybe FixesToApply
validate_exposing_node exposed_val =
    case Node.value exposed_val of
        Exposing.FunctionExpose functionNameStr ->
            let
                node_range =
                    Node.range exposed_val
            in
            if is_invalid_color_str functionNameStr then
                Just <| build_fix exposed_val functionNameStr

            else
                Nothing

        Exposing.InfixExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose _ ->
            Nothing

        Exposing.TypeExpose _ ->
            Nothing


validate_signature : Maybe (Node Signature) -> Fixes
validate_signature signature =
    case signature of
        Just sig_node ->
            sig_node
                |> Node.value
                |> .name
                |> (\sig_name_node ->
                        let
                            sig_name_range =
                                Node.range sig_name_node

                            name : String
                            name =
                                Node.value sig_name_node
                        in
                        [ ( sig_name_range
                          , [ Fix.replaceRangeBy
                                sig_name_range
                                (fix_color_str name)
                            ]
                          )
                        ]
                   )

        Nothing ->
            []


validate_declaration : Node Expression.FunctionImplementation -> Fixes
validate_declaration declaration =
    let
        functionName =
            declaration |> Node.value |> .name

        functionNameStr : String
        functionNameStr =
            declaration |> Node.value |> .name |> Node.value

        node_range =
            Node.range functionName
    in
    [ build_fix_value functionName functionNameStr ]


validate_declaration_with_fix : Node Expression.FunctionImplementation -> FixesToApply
validate_declaration_with_fix declaration =
    let
        functionName =
            declaration |> Node.value |> .name

        functionNameStr : String
        functionNameStr =
            declaration |> Node.value |> .name |> Node.value
    in
    ( functionNameStr, validate_declaration declaration )


declarationExitVisitor : Node Declaration -> AllFixesToApply -> ( List (Error {}), AllFixesToApply )
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, documentation, declaration } ->
            let
                signatureError : Fixes
                signatureError =
                    validate_signature signature

                declarationError : Fixes
                declarationError =
                    validate_declaration declaration

                functionName =
                    declaration |> Node.value |> .name

                functionNameStr : String
                functionNameStr =
                    declaration |> Node.value |> .name |> Node.value
            in
            if is_invalid_color_str functionNameStr then
                ( []
                , Dict.update
                    functionNameStr
                    (\mb_existing_fixes ->
                        let
                            new_entry =
                                declarationError ++ signatureError
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
            )


fixes_to_apply_to_error : FixesToApply -> List (Error {})
fixes_to_apply_to_error ( functionNameStr, range_fixes ) =
    case range_fixes of
        ( first_range, first_fixes ) :: rest_range_and_fixes ->
            [ Rule.errorWithFix
                { message = "Exposed names should start with color_, not end with it"
                , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                }
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
