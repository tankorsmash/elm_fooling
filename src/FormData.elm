module FormData exposing (DataType(..), FormDefinition, FormField, render_fields, unset_string_getter, unset_int_getter,
    unset_float_getter)

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
unset_string_getter _ = ""

unset_int_getter : a -> Int
unset_int_getter _ = 0

unset_float_getter : a -> Float
unset_float_getter _ = 0


type alias FormDefinition a =
    { fields : List (FormField a) }


type DataType
    = StringType
    | IntType
    | FloatType


type alias FormField a =
    { field_name : String
    , data_type : DataType
    , string_getter : Maybe (a -> String)
    , int_getter : Maybe (a -> Int)
    , float_getter : Maybe (a -> Float)

    -- , data_str : String
    -- , data_int : Int
    -- , data_float : Float
    }


render_field : FormField a -> Html msg
render_field field =
    div [] [ text <| "Field name is: " ++ field.field_name ]


render_fields : List (FormField a) -> fd -> Html msg
render_fields fields form_data =
    div [] <| List.map render_field fields
