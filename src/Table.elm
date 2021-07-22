module Table exposing (ColumnDef, ColumnLookup, ColumnType, PageInfo, TableDefinition, view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
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
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (style)
import Utils exposing (add_class)


ellipses_style =
    [ style "max-width" "300px"
    , style "text-overflow" "ellipsis"
    , style "white-space" "nowrap"
    , style "overflow" "hidden"
    ]


type ColumnType
    = ColString
    | ColInt


type alias ColumnDef =
    { column_id : String
    , idx : Int
    , pretty_title : String
    , styles : List ( String, String )
    }


type alias ColumnLookup obj =
    { column_id : String
    , lookup_func : obj -> String
    }



--obj is the type of things in the rows we're holding in the table


type alias TableDefinition =
    { title : Maybe String, columns : List ColumnDef }


build_single_table_header : ( String, List ( String, String ) ) -> Html msg
build_single_table_header ( col_name, col_styles ) =
    let
        styles =
            List.map (\( s, n ) -> style s n) col_styles
    in
    th styles [ text col_name ]


build_table_headers : List ColumnDef -> Html msg
build_table_headers column_datas =
    let
        col_names =
            List.map .pretty_title column_datas

        col_styles =
            List.map .styles column_datas

        table_headers =
            List.map build_single_table_header <| List.map2 Tuple.pair col_names col_styles
    in
    thead [] [ tr [] table_headers ]


build_col : String -> StylePairList -> Html msg
build_col value col_styles =
    let
        styles =
            List.map (\( s, n ) -> style s n) col_styles
    in
    td styles [ text value ]


build_table_row : List String -> List StylePairList -> Html msg
build_table_row row_data col_styles =
    tr [] <| List.map2 build_col row_data col_styles


type alias StylePair =
    ( String, String )


type alias StylePairList =
    List StylePair


type alias PageInfo msg =
    { page_count : Int
    , current_page_idx : Int
    , per_page : Int
    , prev_page_msg : msg
    , next_page_msg : msg
    , change_page_msg : msg
    }


paginate : Int -> Int -> List a -> List a
paginate per_page page_idx rows =
    let
        row_idx =
            per_page * page_idx

        -- given drop 2 [0, 1, 2, 3, 4], we'd have [ 2, 3, 4]
        first_bit =
            List.drop row_idx rows

        -- given [2, 3, 4], we want [2, 3], and leave off anything after
        paginated =
            List.take per_page first_bit
    in
    paginated


{-| Assumes `rows` comes in column-idx order already
-}
view : TableDefinition -> List (List String) -> PageInfo msg -> Html msg
view table_def rows page_info =
    let
        columns =
            table_def.columns

        table_headers =
            [ build_table_headers columns ]

        col_styles : List StylePairList
        col_styles =
            List.map .styles columns

        rows_ : List (List String)
        rows_ =
            paginate page_info.per_page page_info.current_page_idx rows

        row_builder row =
            build_table_row row col_styles

        row_content =
            case rows_ of
                [] ->
                    [ div [] [ text "no content" ] ]

                _ ->
                    List.map row_builder rows_

        create_page_btn page_idx =
            ButtonGroup.button [ Button.outlineDark ] [ text <| "Page " ++ String.fromInt page_idx ]

        page_buttons =
            Grid.row [ Row.centerMd ]
                [ Grid.col [ Col.xs1 ]
                    [ ButtonGroup.buttonGroup [] <|
                        [ ButtonGroup.button [ Button.outlineDark, Button.onClick page_info.prev_page_msg ] [ text "<" ] ]
                            ++ List.map create_page_btn (List.range 1 page_info.page_count)
                            ++ [ ButtonGroup.button [ Button.outlineDark, Button.onClick page_info.next_page_msg ] [ text ">" ] ]
                    ]
                ]

        children =
            table_headers ++ [ tbody [] row_content ]

        table_title =
            case table_def.title of
                Just title ->
                    title

                Nothing ->
                    "TABLE"
    in
    div []
        [ h4 [ style "color" "gray" ] [ text table_title ]
        , table
            [ add_class "table", add_class "table-striped", add_class "table-bordered" ]
            children
        , page_buttons
        ]
