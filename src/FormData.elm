module FormData exposing (DataType(..), FormField, FormDefinition, render_fields)

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

type alias FormDefinition =
    { fields : List FormField }


type DataType
    = String
    | Int
    | Float


type alias FormField =
    { field_name : String
    , data_type : DataType
    -- , data_str : String
    -- , data_int : Int
    -- , data_float : Float
    }

render_field : FormField -> Html msg
render_field field =
    div [] [text <| "Field name is: " ++ field.field_name]

render_fields : List FormField -> fd -> Html msg
render_fields fields form_data =
    div [] <| List.map render_field fields
