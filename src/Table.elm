module Table exposing (ColumnDef, ColumnType, TableDefinition, view)

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
        , span
        , table
        , td
        , text
        , th
        , thead
        , tr
        )


type ColumnType
    = ColString
    | ColInt


type alias ColumnDef a =
    { column_id : String
    -- , column_lookup : obj -> String
    , column_lookup : { a | title : String } -> String
    , idx : Int
    , pretty_title : String
    }



--obj is the type of things in the rows we're holding in the table


type alias TableDefinition obj =
    { columns : List (ColumnDef obj) }


build_single_table_header : String -> Html msg
build_single_table_header col_name =
    th []
        [ td []
            [ text col_name ]
        ]


build_table_headers : List (ColumnDef obj) -> Html msg
build_table_headers column_datas =
    let
        col_names =
            List.map .pretty_title column_datas

        table_headers =
            List.map build_single_table_header col_names
    in
    tr [] table_headers


build_cell : String -> Html msg
build_cell value =
    td [] [ text value ]


build_table_row : List String -> Html msg
build_table_row row_data =
    tr [] <| List.map build_cell row_data



-- column_getter : ColumnDef obj -> obj -> String
-- column_getter col_def obj_ =
--     -- TODO: look up the col_def.column_id in object
--     obj_ col_def.column_id



view : TableDefinition obj -> List (List String) -> Html msg
view table_def rows =
    let
        columns = table_def.columns

        children =
            [ build_table_headers columns ]
                ++ List.map build_table_row rows
    in
    table [] children
