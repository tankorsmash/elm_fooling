module FormData exposing
    ( DataType(..)
    , FormDefinition
    , FormField
    , new_form_field_enum
    , new_form_field_float
    , new_form_field_int
    , new_form_field_string
    , render_fields
    , unset_float_getter
    , unset_int_getter
    , unset_string_getter
    , update_enum_field
    , update_int_field
    )

import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html
    exposing
        ( Html
        , a
        , b
        , br
        , button
        , div
        , h1
        , h2
        , h3
        , h4
        , input
        , label
        , p
        , span
        , table
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, for, value)



{- the type of data in the form field -}


unset_string_getter : a -> String
unset_string_getter _ =
    ""


unset_int_getter : a -> Int
unset_int_getter _ =
    0


unset_float_getter : a -> Float
unset_float_getter _ =
    0


type alias FormDefinition fd msg =
    { fields : List (FormField fd msg) }


type DataType
    = StringType
    | IntType
    | FloatType
    | EnumType


{-| Returns either the int value of maybe\_new\_val, or the fallback
-}
update_int_field : Int -> String -> Int
update_int_field fallback maybe_new_val =
    case String.toInt maybe_new_val of
        Just new_val ->
            new_val

        Nothing ->
            fallback


update_enum_field : a -> String -> (Int -> a) -> a
update_enum_field fallback maybe_new_val int_to_enum =
    case String.toInt maybe_new_val of
        Just new_int_enum_val ->
            int_to_enum new_int_enum_val

        Nothing ->
            fallback


to_string : DataType -> String
to_string dtype =
    case dtype of
        StringType ->
            "StringType"

        IntType ->
            "IntType"

        FloatType ->
            "FloatType"

        EnumType ->
            "EnumType"


type alias FormField fd msg =
    { field_name : String
    , data_type : DataType
    , string_getter : Maybe (fd -> String)
    , int_getter : Maybe (fd -> Int)
    , float_getter : Maybe (fd -> Float)
    , enum_getter : Maybe (fd -> String) --In the field definition, we need to convert the enum to a string ourselves, and pass that string from the getter
    , enum_values : Maybe (List (String, String)) -- [(val, text), (val, text)]
    , on_input_msg : String -> msg
    }


render_field_input_string : fd -> FormField fd msg -> Html msg
render_field_input_string obj field =
    InputGroup.config
        (InputGroup.text
            [ Input.placeholder "placeholder"
            , Input.value <|
                case field.string_getter of
                    Just getter ->
                        getter obj

                    Nothing ->
                        "unset in field"
            , Input.onInput field.on_input_msg
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text field.field_name ] ]
        |> InputGroup.view


render_field_input_enum : fd -> FormField fd msg -> Html msg
render_field_input_enum obj field =
    -- Form.group [Form.attrs [class ""]]
    --     [ Form.label [ for field.field_name, class "mr-3" ] [ text field.field_name ]
    --     , Select.select [ Select.id field.field_name ]
    --         [ Select.item [] [ text "TODO" ]
    --         , Select.item [] [ text "REPLACE" ]
    --         , Select.item [] [ text "ME" ]
    --         ]
    --     ]
    div [ class "input-group" ]
        [ div [ class "input-group-prepend" ]
            [ Form.label [ for field.field_name, class "input-group-text" ] [ text field.field_name ]
            ]
        , Select.custom [ Select.id field.field_name ] <|
            case field.enum_values of
                Nothing -> Debug.log "Nothing" []
                Just values -> Debug.log "values" List.map (\(v, t) -> Select.item [value v] [ text t]) values
            -- [ Select.item [] [ text "TODO" ]
            -- , Select.item [] [ text "REPLACE" ]
            -- , Select.item [] [ text "ME" ]
            -- ]
        ]



-- <div class="input-group mb-3">
--   <div class="input-group-prepend">
--     <label class="input-group-text" for="inputGroupSelect01">Options</label>
--   </div>
--   <select class="custom-select" id="inputGroupSelect01">
--     <option selected>Choose...</option>
--     <option value="1">One</option>
--     <option value="2">Two</option>
--     <option value="3">Three</option>
--   </select>
-- </div>
-- InputGroup.config
--     (InputGroup.text
--         [ Input.placeholder "placeholder"
--         , Select.select [ ] [Select.item [] [] ]
--         -- , Input.value <|
--         --     case field.enum_getter of
--         --         Just getter ->
--         --             getter obj
--         --
--         --         Nothing ->
--         --             "unset in field"
--         , Input.onInput field.on_input_msg
--         ]
--     )
--     |> InputGroup.predecessors
--         [ InputGroup.span [] [ text field.field_name ] ]
--     |> InputGroup.view


render_field_input_number : fd -> FormField fd msg -> Html msg
render_field_input_number obj field =
    InputGroup.config
        (InputGroup.number
            [ Input.placeholder "placeholder"
            , Input.value <|
                case field.int_getter of
                    Just getter ->
                        String.fromInt <| getter obj

                    Nothing ->
                        "-1233333"
            , Input.onInput field.on_input_msg
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text field.field_name ] ]
        |> InputGroup.view


lookup_field : fd -> FormField fd msg -> String
lookup_field obj field =
    case field.data_type of
        StringType ->
            case field.string_getter of
                Just getter ->
                    getter obj

                Nothing ->
                    "unset in lookup"

        IntType ->
            String.fromInt <|
                case field.int_getter of
                    Just getter ->
                        getter obj

                    Nothing ->
                        0

        FloatType ->
            String.fromFloat <|
                case field.float_getter of
                    Just getter ->
                        getter obj

                    Nothing ->
                        0.0

        EnumType ->
            case field.enum_getter of
                Just getter ->
                    getter obj

                Nothing ->
                    "unset enum in lookup"


render_field_to_plaintext : fd -> FormField fd msg -> Html msg
render_field_to_plaintext obj field =
    div [] [ text <| "Field name is: " ++ field.field_name ++ ", " ++ lookup_field obj field ]


render_field : fd -> FormField fd msg -> Html msg
render_field obj field =
    case field.data_type of
        IntType ->
            div [] [ render_field_input_number obj field ]

        FloatType ->
            div [] [ render_field_input_number obj field ]

        StringType ->
            div [] [ render_field_input_string obj field ]

        EnumType ->
            div [] [ render_field_input_enum obj field ]


render_fields : List (FormField fd msg) -> fd -> Html msg
render_fields fields form_data =
    div [] <|
        List.map
            -- (render_field_to_plaintext form_data)
            (render_field form_data)
            fields


new_form_field_int : String -> (fd -> Int) -> (String -> msg) -> FormField fd msg
new_form_field_int name getter on_input_msg =
    { field_name = name
    , data_type = IntType
    , string_getter = Nothing
    , int_getter = Just getter
    , float_getter = Nothing
    , enum_getter = Nothing
    , enum_values = Nothing
    , on_input_msg = on_input_msg
    }


new_form_field_enum : String -> (fd -> String) -> (String -> msg) -> List (String, String) -> FormField fd msg
new_form_field_enum name getter on_input_msg enum_values =
    { field_name = name
    , data_type = EnumType
    , string_getter = Nothing
    , int_getter = Nothing
    , float_getter = Nothing
    , enum_getter = Just getter
    , enum_values = Just enum_values
    , on_input_msg = on_input_msg
    }


new_form_field_string : String -> (fd -> String) -> (String -> msg) -> FormField fd msg
new_form_field_string name getter on_input_msg =
    { field_name = name
    , data_type = StringType
    , string_getter = Just getter
    , int_getter = Nothing
    , float_getter = Nothing
    , enum_getter = Nothing
    , enum_values = Nothing
    , on_input_msg = on_input_msg
    }


new_form_field_float : String -> (fd -> Float) -> (String -> msg) -> FormField fd msg
new_form_field_float name getter on_input_msg =
    { field_name = name
    , data_type = FloatType
    , string_getter = Nothing
    , int_getter = Nothing
    , float_getter = Just getter
    , enum_getter = Nothing
    , enum_values = Nothing
    , on_input_msg = on_input_msg
    }
