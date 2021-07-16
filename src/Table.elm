module Table exposing (view)

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


build_single_table_header : String -> Html msg
build_single_table_header col_name =
    th []
        [ td []
            [ text col_name ]
        ]


build_table_headers : List String -> Html msg
build_table_headers col_names =
    let
        table_headers =
            List.map build_single_table_header col_names
    in
    tr [] table_headers


build_cell : String -> Html msg
build_cell value =
    td [] [ text value ]


build_data_row : List String -> Html msg
build_data_row row_data =
    tr [] <| List.map build_cell row_data


view : List String -> List (List String) -> Html msg
view columns rows =
    let
        children =
            [ build_table_headers columns ]
                ++ List.map build_data_row rows
    in
    table [] children
