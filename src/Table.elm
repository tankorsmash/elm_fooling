module Table exposing
    ( ColumnDef
    , ColumnLookup
    , ColumnType
    , PageInfo
    , TableDefinition
    , decrement_page_idx
    , increment_page_idx
    , initialize_page_info
    , view
    )

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

    {- Takes an page_idx and returns a msg type -}
    , change_page_msg : Int -> msg
    }



{- Sets the page count based on per_page and the rows passed in -}


initialize_page_info : PageInfo msg -> List a -> PageInfo msg
initialize_page_info page_info rows =
    let
        num_rows =
            List.length rows

        new_page_count =
            floor <| (toFloat <| num_rows) / toFloat page_info.per_page

        lg =
            Debug.log "Page count" new_page_count

        lg2 =
            Debug.log "Num rows" num_rows
    in
    { page_info | page_count = new_page_count }


increment_page_idx : PageInfo msg -> PageInfo msg
increment_page_idx page_info =
    let
        per_page =
            page_info.per_page

        page_idx =
            page_info.current_page_idx

        page_count =
            page_info.page_count

        new_page_idx =
            page_idx + 1
    in
    if new_page_idx > page_count then
        page_info

    else
        { page_info | current_page_idx = new_page_idx }


decrement_page_idx : PageInfo msg -> PageInfo msg
decrement_page_idx page_info =
    let
        per_page =
            page_info.per_page

        page_idx =
            page_info.current_page_idx

        page_count =
            page_info.page_count

        new_page_idx =
            page_idx - 1
    in
    if new_page_idx < 0 then
        page_info

    else
        { page_info | current_page_idx = new_page_idx }


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
            let
                btn_style =
                    if page_info.current_page_idx == page_idx then
                        Button.primary

                    else
                        Button.outlineDark
            in
            ButtonGroup.button
                [ btn_style
                , Button.onClick <| page_info.change_page_msg page_idx
                ]
                [ text <| "Page " ++ String.fromInt (page_idx + 1) ]

        prev_btn =
            let
                btn_attrs =
                    if page_info.current_page_idx /= 0 then
                        Button.attrs []

                    else
                        Button.attrs [ style "visibility" "hidden" ]
            in
            [ ButtonGroup.button
                [ Button.outlineSecondary
                , Button.onClick page_info.prev_page_msg
                , btn_attrs
                ]
                [ text "<" ]
            ]

        inner_page_btns =
            if page_info.page_count /= 0 then
                List.map create_page_btn (List.range 0 page_info.page_count)

            else
                []

        next_btn =
            let
                btn_attrs =
                    if page_info.current_page_idx /= 0 then
                        Button.attrs []

                    else
                        Button.attrs [ style "visibility" "hidden" ]
            in
            [ ButtonGroup.button
                [ Button.outlineSecondary
                , Button.onClick page_info.next_page_msg
                , btn_attrs
                ]
                [ text ">" ]
            ]

        page_buttons =
            Grid.row [ Row.centerMd ]
                [ Grid.col [ Col.mdAuto ]
                    [ ButtonGroup.buttonGroup [] <|
                        prev_btn
                            ++ inner_page_btns
                            ++ next_btn
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
