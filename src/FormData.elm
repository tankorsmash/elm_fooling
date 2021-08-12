module FormData exposing
    ( DataType(..)
    , EnumAccessor
    , FormDefinition
    , FormField
    , InputCallback
    , ListFieldAlterType(..)
    , ignore_alter
    , new_form_field_enum
    , new_form_field_float
    , new_form_field_int
    , new_form_field_list_string
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
import Html.Attributes exposing (class, for, selected, value)



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


type alias EnumAccessor fd =
    fd -> String


type alias InputCallback msg =
    String -> msg


{-| since list field types have buttons to add and remove fields, these should be attached to the generic FormUpdate msg they send
-}
type ListFieldAlterType
    = Add
    | Remove
    | Change


ignore_alter : a -> ListFieldAlterType -> a
ignore_alter rest _ =
    rest


type DataType fd
    = StringType (fd -> String)
    | IntType (fd -> Int)
    | FloatType (fd -> Float)
    | EnumType (EnumAccessor fd) --since C++ uses Ints as json values for enums, we need to give it a converter to go from string to enum's int
    | ListStringType (fd -> List String)


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


to_string : DataType a -> String
to_string dtype =
    case dtype of
        StringType _ ->
            "StringType"

        IntType _ ->
            "IntType"

        FloatType _ ->
            "FloatType"

        EnumType _ ->
            "EnumType"

        ListStringType _ ->
            "ListStringType"


type alias FormField fd msg =
    { field_name : String
    , data_type : DataType fd
    , enum_values : Maybe (List ( String, String )) -- [(val, text), (val, text)]
    , on_input_msg : ListFieldAlterType -> InputCallback msg
    }


render_field_input_string : fd -> FormField fd msg -> (fd -> String) -> Html msg
render_field_input_string obj field getter =
    InputGroup.config
        (InputGroup.text
            [ Input.placeholder "placeholder"
            , Input.value <| getter obj
            , Input.onInput <| field.on_input_msg <| Change
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text field.field_name ] ]
        |> InputGroup.view


render_field_input_list_string : fd -> FormField fd msg -> (fd -> List String) -> Html msg
render_field_input_list_string obj field getter =
    let
        vals_to_string : List String -> String
        vals_to_string v =
            String.join ", " <| v

        values : List String
        values =
            getter obj

        rendered_list =
            vals_to_string <| values

        --takes a substring and returns the FULL field value but with this idx swapped out
        replace_value : Int -> String -> String
        replace_value idx str =
            vals_to_string <| List.take idx values ++ str :: List.drop (idx + 1) values

        split_msg : Int -> (String -> msg)
        split_msg idx =
            \str -> (field.on_input_msg <| Change) <| replace_value idx str

        build_config : String -> Int -> InputGroup.Config msg
        build_config value idx =
            InputGroup.config <|
                InputGroup.text
                    [ Input.placeholder "placeholder"
                    , Input.value <| value
                    , Input.onInput <| split_msg idx
                    ]

        build_pred : String -> Int -> InputGroup.Config msg -> InputGroup.Config msg
        build_pred field_name idx cfg =
            cfg
                |> InputGroup.predecessors
                    [ InputGroup.span [] [ text <| field_name ++ " #" ++ String.fromInt idx ] ]

        build_input_group : Int -> String -> String -> Html msg
        build_input_group idx value field_name =
            build_config value idx
                |> build_pred field_name idx
                |> InputGroup.view
    in
    div [] <| List.indexedMap (\idx val -> build_input_group idx val field.field_name) values



-- build_msg : String -> (update_msg -> msg) -> (String -> msg)
-- build_msg : (FormField fd msg) -> String -> (update_msg -> msg) -> (String -> msg)
-- build_msg field str on_input_msg =
--     -- field.on_input_msg str
--     let callback : String -> msg
--         -- callback = \s -> (field.on_input_msg s)
--         callback = field.on_input_msg str
--     in callback
-- build_dumb_msg : String -> msg
-- build_dumb_msg str = Msg
--


render_field_input_enum : fd -> FormField fd msg -> EnumAccessor fd -> Html msg
render_field_input_enum obj field getter =
    div [ class "input-group" ]
        [ div [ class "input-group-prepend" ]
            [ Form.label
                [ for field.field_name
                , class "input-group-text"
                ]
                [ text field.field_name
                ]
            ]
        , Select.custom
            [ Select.id field.field_name
            , Select.onChange (field.on_input_msg <| Change)
            ]
          <|
            case field.enum_values of
                Nothing ->
                    -- Debug.log "Nothing" <|
                    []

                Just values ->
                    -- Debug.log "values" <|
                    List.map
                        (\( v, t ) ->
                            Select.item
                                [ value v, selected (getter obj == t) ]
                                [ text t ]
                        )
                        values
        ]


render_field_input_int : fd -> FormField fd msg -> (fd -> Int) -> Html msg
render_field_input_int obj field getter =
    InputGroup.config
        (InputGroup.number
            [ Input.placeholder "placeholder"
            , Input.value <| String.fromInt <| getter obj
            , Input.onInput <| field.on_input_msg <| Change
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text field.field_name ] ]
        |> InputGroup.view


render_field_input_float : fd -> FormField fd msg -> (fd -> Float) -> Html msg
render_field_input_float obj field getter =
    InputGroup.config
        (InputGroup.number
            [ Input.placeholder "placeholder"
            , Input.value <| String.fromFloat <| getter obj
            , Input.onInput <| field.on_input_msg <| Change
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text field.field_name ] ]
        |> InputGroup.view


lookup_field : fd -> FormField fd msg -> String
lookup_field obj field =
    case field.data_type of
        StringType getter ->
            getter obj

        IntType getter ->
            String.fromInt <| getter obj

        FloatType getter ->
            String.fromFloat <| getter obj

        EnumType getter ->
            getter obj

        ListStringType getter ->
            String.join ", " <| getter obj


render_field_to_plaintext : fd -> FormField fd msg -> Html msg
render_field_to_plaintext obj field =
    div [] [ text <| "Field name is: " ++ field.field_name ++ ", " ++ lookup_field obj field ]


render_field : fd -> FormField fd msg -> Html msg
render_field obj field =
    case field.data_type of
        IntType getter ->
            div [] [ render_field_input_int obj field getter ]

        FloatType getter ->
            div [] [ render_field_input_float obj field getter ]

        StringType getter ->
            div [] [ render_field_input_string obj field getter ]

        EnumType getter ->
            div [] [ render_field_input_enum obj field getter ]

        ListStringType getter ->
            div [] [ render_field_input_list_string obj field getter ]


render_fields : List (FormField fd msg) -> fd -> Html msg
render_fields fields form_data =
    div [] <|
        List.map
            -- (render_field_to_plaintext form_data)
            (render_field form_data)
            fields


new_form_field_int : String -> (fd -> Int) -> (ListFieldAlterType -> InputCallback msg) -> FormField fd msg
new_form_field_int name getter on_input_msg =
    { field_name = name
    , data_type = IntType getter
    , enum_values = Nothing
    , on_input_msg = on_input_msg
    }


new_form_field_enum : String -> EnumAccessor fd -> (ListFieldAlterType -> InputCallback msg) -> List ( String, String ) -> FormField fd msg
new_form_field_enum name accessor on_input_msg enum_values =
    { field_name = name
    , data_type = EnumType accessor
    , enum_values = Just enum_values
    , on_input_msg = on_input_msg
    }


new_form_field_string : String -> (fd -> String) -> (ListFieldAlterType -> InputCallback msg) -> FormField fd msg
new_form_field_string name getter on_input_msg =
    { field_name = name
    , data_type = StringType getter
    , enum_values = Nothing
    , on_input_msg = on_input_msg
    }


new_form_field_float : String -> (fd -> Float) -> (ListFieldAlterType -> InputCallback msg) -> FormField fd msg
new_form_field_float name getter on_input_msg =
    { field_name = name
    , data_type = FloatType getter
    , enum_values = Nothing
    , on_input_msg = on_input_msg
    }


new_form_field_list_string :
    String
    -> (fd -> List String)
    -> (ListFieldAlterType -> InputCallback msg)
    -> FormField fd msg
new_form_field_list_string name getter on_input_msg =
    { field_name = name
    , data_type = ListStringType getter
    , enum_values = Nothing
    , on_input_msg = on_input_msg
    }
