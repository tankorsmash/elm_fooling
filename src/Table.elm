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
            List.map .column_id column_datas

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


do_lookups : List (a -> String) -> a -> List String
do_lookups lookups row =
    []


view : TableDefinition obj -> List obj -> Html msg
view table_def rows =
    let
        columns =
            table_def.columns

        -- single_col = case (List.head columns) of
        --     Nothing -> { column_loop = "" }
        --     Just col -> col
        lookups =
            List.map .column_lookup columns

        -- row_strings =
        -- List.map (List.take 1 columns).column_lookup rows
        -- List.map single_col.column_lookup rows
        --HACK to just get a first version working
        children =
            [ build_table_headers columns ]
                -- for each row of data, turn it into a <tr/>
                ++ List.map build_table_row
                    -- this returns a list of strings for each passed-in row
                    --  for each column in the row
                    -- (List.map (\row -> List.map lookups row) rows )
                    (List.map (do_lookups lookups) rows)
    in
    table [] children
