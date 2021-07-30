module FormData exposing
    ( DataType(..)
    , FormDefinition
    , FormField
    , render_fields
    , unset_float_getter
    , unset_int_getter
    , unset_string_getter
    )

import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
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
        , p
        , span
        , table
        , td
        , text
        , th
        , thead
        , tr
        )



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


type alias FormDefinition fd =
    { fields : List (FormField fd) }


type DataType
    = StringType
    | IntType
    | FloatType

to_string : DataType -> String
to_string dtype = case dtype of
    StringType -> "StringType"
    IntType -> "IntType"
    FloatType -> "FloatType"


type alias FormField fd =
    { field_name : String
    , data_type : DataType
    , string_getter : Maybe (fd -> String)
    , int_getter : Maybe (fd -> Int)
    , float_getter : Maybe (fd -> Float)
    }



render_field_input_number : fd -> FormField fd -> Html msg
render_field_input_number obj field =
    InputGroup.config
        (InputGroup.number
            [ Input.placeholder "placeholder"
            , Input.value "some value"

            -- , Input.onInput some_msg
            ]
        )
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text field.field_name] ]
        |> InputGroup.view


lookup_field : fd -> FormField fd -> String
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


render_field_to_plaintext : fd -> FormField fd -> Html msg
render_field_to_plaintext obj field =
    div [] [ text <| "Field name is: " ++ field.field_name ++ ", " ++ lookup_field obj field ]


render_field : fd -> FormField fd -> Html msg
render_field obj field =
    case field.data_type of
        IntType -> div [] [ render_field_input_number obj field ]
        unknown_type -> div [] [ text <| "unknown field type: "++to_string unknown_type ]


render_fields : List (FormField fd) -> fd -> Html msg
render_fields fields form_data =
    div [] <|
        List.map
            -- (render_field_to_plaintext form_data)
            (render_field form_data)
            fields
            -- ++ [ render_field_input_number ]
