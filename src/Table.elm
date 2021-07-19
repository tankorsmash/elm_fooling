module Table exposing (ColumnDef, ColumnLookup, ColumnType, TableDefinition, view)

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
import Html.Attributes exposing (style)


type ColumnType
    = ColString
    | ColInt


type alias ColumnDef =
    { column_id : String
    , idx : Int
    , pretty_title : String
    }


type alias ColumnLookup obj =
    { column_id : String
    , lookup_func : obj -> String
    }



--obj is the type of things in the rows we're holding in the table


type alias TableDefinition =
    { title : Maybe String, columns : List ColumnDef }


build_single_table_header : String -> Html msg
build_single_table_header col_name =
    th []
        [ td []
            [ text col_name ]
        ]


build_table_headers : List ColumnDef -> Html msg
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


view : TableDefinition -> List (List String) -> Html msg
view table_def rows =
    let
        columns =
            table_def.columns

        table_headers =
            [ build_table_headers columns ]

        row_content =
            case rows of
                [] ->
                    [ div [] [ text "no content" ] ]

                _ ->
                    List.map build_table_row rows

        children =
            table_headers ++ row_content

        table_title = case table_def.title of
            Just title -> title
            Nothing -> "TABLE"
    in
    div []
        [ h4 [ style "color" "gray" ] [ text table_title ]
        , table [] children
        ]
