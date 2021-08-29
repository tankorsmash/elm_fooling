module Table exposing
    ( ColumnDef
    , ColumnType(..)
    , PageInfo
    , PageInfoMsg
    , TableDefinition
    , decrement_page_idx
    , increment_page_idx
    , initialize_page_info
    , new_page_info
    , update_page_info
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
        , img
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
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Utils exposing (add_class)


ellipses_style =
    [ style "max-width" "300px"
    , style "text-overflow" "ellipsis"
    , style "white-space" "nowrap"
    , style "overflow" "hidden"
    ]


type ColumnType
    = String
    | Int
    | Img


type PageInfoMsg
    = PrevPageMsg
    | NextPageMsg
    | ChangePageMsg Int


type alias ColumnDef obj =
    { column_id : String
    , idx : Int
    , pretty_title : String
    , styles : List ( String, String )
    , lookup_func : obj -> String
    , column_type : ColumnType
    }



-- type alias ColumnLookup obj =
--     { column_id : String
--     , lookup_func : obj -> String
--     }
--obj is the type of things in the rows we're holding in the table


type alias TableDefinition obj =
    { title : Maybe String, columns : List (ColumnDef obj) }


build_single_table_header : ( String, List ( String, String ) ) -> Html msg
build_single_table_header ( col_name, col_styles ) =
    let
        styles =
            List.map (\( s, n ) -> style s n) col_styles
    in
    th styles [ text col_name ]


build_table_headers : List (ColumnDef obj) -> Html msg
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


build_col : String -> ColumnDef obj -> Html msg
build_col value col_def =
    let
        styles =
            List.map (\( s, n ) -> style s n) col_def.styles

        rendered_col =
            case col_def.column_type of
                String ->
                    text value

                Int ->
                    text value

                Img ->
                    img [ add_class "img-fluid", src <| value ] []
    in
    td styles [ rendered_col ]


build_table_row : List String -> ColumnDefList obj -> Html msg
build_table_row row_data columns =
    tr [] <| List.map2 build_col row_data columns


type alias StylePair =
    ( String, String )


type alias StylePairList =
    List StylePair


type alias ColumnDefList obj =
    List (ColumnDef obj)


type alias PageInfo msg =
    { page_count : Int
    , current_page_idx : Int
    , per_page : Int
    , custom_type : PageInfoMsg -> msg
    }


update_page_info : PageInfo msg -> PageInfoMsg -> PageInfo msg
update_page_info page_info page_msg =
    case page_msg of
        PrevPageMsg ->
            decrement_page_idx page_info

        NextPageMsg ->
            increment_page_idx page_info

        ChangePageMsg page_idx ->
            change_page_idx page_info page_idx


new_page_info : (PageInfoMsg -> msg) -> PageInfo msg
new_page_info handler =
    PageInfo 0 0 10 handler


{-| Sets the page count based on per\_page and the rows passed in
-}
initialize_page_info : PageInfo msg -> List a -> PageInfo msg
initialize_page_info page_info rows =
    let
        num_rows =
            List.length rows

        new_page_count =
            floor <| (toFloat <| num_rows) / toFloat page_info.per_page
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


change_page_idx : PageInfo msg -> Int -> PageInfo msg
change_page_idx page_info new_page_idx =
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


build_rows : List (ColumnDef obj) -> obj -> List String
build_rows column_defs row =
    let
        cell_builder cl =
            case cl.column_type of
                String ->
                    cl.lookup_func row

                Int ->
                    cl.lookup_func row

                Img ->
                    cl.lookup_func row
    in
    List.foldl
        (\cl acc ->
            acc
                ++ [ cell_builder cl ]
        )
        []
    <|
        List.sortBy .idx column_defs


build_prev_page_msg : PageInfo msg -> msg
build_prev_page_msg page_info =
    page_info.custom_type PrevPageMsg


build_next_page_msg : PageInfo msg -> msg
build_next_page_msg page_info =
    page_info.custom_type NextPageMsg


build_change_page_msg : PageInfo msg -> Int -> msg
build_change_page_msg page_info page_idx =
    page_info.custom_type (ChangePageMsg page_idx)


{-| Assumes `rows` comes in column-idx order already
-}
view : TableDefinition obj -> List obj -> PageInfo msg -> Html msg
view table_def unsorted_rows page_info =
    let
        columns =
            List.sortBy .idx table_def.columns

        no_col_def_warning =
            if List.length columns == 0 then
                h1 [] [ text "No column definitions passed in" ]

            else
                span [] []

        sorted_rows =
            List.map (build_rows columns) unsorted_rows

        table_headers =
            [ build_table_headers columns ]

        -- col_styles : List StylePairList
        -- col_styles =
        --     List.map .styles columns
        --
        paginated_rows : List (List String)
        paginated_rows =
            let
                per_page =
                    page_info.per_page

                rows =
                    paginate page_info.per_page page_info.current_page_idx sorted_rows
            in
            case per_page - List.length rows of
                0 ->
                    rows

                num_missing ->
                    rows
                        ++ (let
                                dummy_row =
                                    List.map (\_ -> "-") <| List.range 0 (List.length columns - 1)

                                val =
                                    List.map (\_ -> dummy_row) <| List.range 0 (num_missing - 1)
                            in
                            val
                           )

        row_builder : List String -> Html msg
        row_builder row =
            build_table_row row columns

        no_content_warning =
            [ tr [] [ td [] [ text "no content" ] ] ]

        row_content : List (Html msg)
        row_content =
            case paginated_rows of
                [] ->
                    no_content_warning

                rows ->
                    if List.all (\el -> List.length el == 0) rows then
                        no_content_warning

                    else
                        List.map row_builder paginated_rows

        create_page_btn : Int -> ButtonGroup.ButtonItem msg
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

                -- , Button.onClick <| page_info.change_page_msg page_idx
                , Button.onClick <| build_change_page_msg page_info page_idx
                , Button.attrs [ onMouseEnter <| build_change_page_msg page_info page_idx ]
                ]
                [ text <| "Page " ++ String.fromInt (page_idx + 1) ]

        prev_btn : List (ButtonGroup.ButtonItem msg)
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

                -- , Button.onClick page_info.prev_page_msg
                , Button.onClick <| build_prev_page_msg page_info
                , btn_attrs
                ]
                [ text "<" ]
            ]

        inner_page_btns : List (ButtonGroup.ButtonItem msg)
        inner_page_btns =
            if page_info.page_count /= 0 then
                List.map create_page_btn (List.range 0 page_info.page_count)

            else
                []

        next_btn : List (ButtonGroup.ButtonItem msg)
        next_btn =
            let
                btn_attrs =
                    if page_info.current_page_idx /= page_info.page_count then
                        Button.attrs []

                    else
                        Button.attrs [ style "visibility" "hidden" ]
            in
            [ ButtonGroup.button
                [ Button.outlineSecondary

                -- , Button.onClick page_info.next_page_msg
                , Button.onClick <| build_next_page_msg page_info
                , btn_attrs
                ]
                [ text ">" ]
            ]

        page_buttons : Html msg
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
        , no_col_def_warning
        , table
            [ add_class "table", add_class "table-striped", add_class "table-bordered" ]
            children
        , page_buttons
        ]
