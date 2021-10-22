module ItemShop exposing (Model, Msg, init, update, view)

import Color
import Color.Convert as Convert
import Element
    exposing
        ( Color
        , Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , explain
        , fill
        , fillPortion
        , height
        , modular
        , padding
        , paddingXY
        , paragraph
        , rgb
        , rgb255
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes


type ItemType
    = Weapon
    | Armor
    | Spellbook
    | Furniture
    | Food


type alias Item =
    { name : String
    , item_type : ItemType
    , gold_cost : Int
    , description : String
    }


type ListContext
    = ShopItems
    | InventoryItems


type Msg
    = Noop
    | MouseEnterShopItem ListContext Item
    | MouseLeaveShopItem ListContext Item
    | BuyItem Item Int
    | SellItem Item Int


type alias InventoryRecord =
    ( Item, Int )


type alias InventoryRecords =
    List InventoryRecord


type alias Model =
    { owned_items : List ( Item, Int )
    , items_for_sale : List ( Item, Int )
    , hovered_item_for_sale : Maybe Item
    , hovered_item_in_inventory : Maybe Item
    , gold_in_pocket : Int
    }


initial_items_for_sale : List ( Item, Int )
initial_items_for_sale =
    [ ( { name = "Boots", item_type = Armor, gold_cost = 5, description = "An old pair of boots" }, 3 )
    , ( { name = "Sword", item_type = Weapon, gold_cost = 15, description = "A rusted sword" }, 5 )
    , ( { name = "Dagger", item_type = Weapon, gold_cost = 10, description = "A small weapon that fits in your pocket" }, 4 )
    , ( { name = "Book of the Dead with a real", item_type = Spellbook, gold_cost = 20, description = "Bound in leather, this book has a skull for a cover" }, 1 )
    ]


initial_owned_items : List ( Item, Int )
initial_owned_items =
    [ ( { name = "Boots", item_type = Armor, gold_cost = 5, description = "An old pair of boots" }, 10 )
    ]


get_adjusted_item_cost : Item -> Int -> Int
get_adjusted_item_cost item qty =
    item.gold_cost * qty


white_color : Color
white_color =
    rgb 1 1 1


secondary_color : Color
secondary_color =
    case Convert.hexToColor "#6c757d" of
        Ok color ->
            let
                -- convert to a Color lib Color record
                rgba =
                    Color.toRgba color
            in
            -- from the Color record, call the ElmUI `rgb` func
            rgb rgba.red rgba.green rgba.blue

        Err err ->
            rgb255 255 0 0


danger_color : Color
danger_color =
    case Convert.hexToColor "#dc3545" of
        Ok color ->
            let
                -- convert to a Color lib Color record
                rgba =
                    Color.toRgba color
            in
            -- from the Color record, call the ElmUI `rgb` func
            rgb rgba.red rgba.green rgba.blue

        Err err ->
            rgb255 255 0 0


primary_color : Color
primary_color =
    case Convert.hexToColor "#007bff" of
        Ok color ->
            let
                -- convert to a Color lib Color record
                rgba =
                    Color.toRgba color
            in
            -- from the Color record, call the ElmUI `rgb` func
            rgb rgba.red rgba.green rgba.blue

        Err err ->
            rgb255 255 0 0


primary_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
primary_button attrs on_press label =
    Input.button
        ([ -- bs4-like values
           Font.color white_color
         , Font.size 16
         , Font.center
         , padding 6
         , Background.color primary_color
         , Border.rounded 5
         , Border.width 5
         , Border.color primary_color
         ]
            ++ attrs
        )
        { onPress = Just on_press, label = text label }


secondary_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
secondary_button attrs on_press label =
    Input.button
        ([ -- bs4-like values
           Font.color white_color
         , Font.size 16
         , Font.center
         , padding 6
         , Background.color secondary_color
         , Border.rounded 5
         , Border.width 5
         , Border.color secondary_color
         ]
            ++ attrs
        )
        { onPress = Just on_press, label = text label }


danger_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
danger_button attrs on_press label =
    Input.button
        ([ -- bs4-like values
           Font.color white_color
         , Font.size 16
         , Font.center
         , padding 6
         , Background.color danger_color
         , Border.rounded 5
         , Border.width 5
         , Border.color danger_color
         ]
            ++ attrs
        )
        { onPress = Just on_press, label = text label }


item_type_to_pretty_string : ItemType -> String
item_type_to_pretty_string item_type =
    case item_type of
        Weapon ->
            "Weapon"

        Armor ->
            "Armor"

        Spellbook ->
            "Spellbook"

        Furniture ->
            "Furniture"

        Food ->
            "Food"


init : Model
init =
    { owned_items = initial_owned_items
    , items_for_sale = initial_items_for_sale
    , hovered_item_for_sale = Nothing
    , hovered_item_in_inventory = Nothing
    , gold_in_pocket = 0
    }


find_matching_records : Item -> InventoryRecord -> Bool
find_matching_records to_match ( recorded_item, _ ) =
    to_match == recorded_item


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        MouseEnterShopItem context item ->
            case context of
                ShopItems ->
                    ( { model | hovered_item_for_sale = Just item }, Cmd.none )

                InventoryItems ->
                    ( { model | hovered_item_in_inventory = Just item }, Cmd.none )

        MouseLeaveShopItem context item ->
            case context of
                ShopItems ->
                    ( { model | hovered_item_for_sale = Nothing }, Cmd.none )

                InventoryItems ->
                    ( { model | hovered_item_in_inventory = Nothing }, Cmd.none )

        BuyItem item qty ->
            let
                total_cost =
                    get_adjusted_item_cost item qty

                can_afford_item =
                    total_cost <= model.gold_in_pocket

                reduce_if_matched ( i, iq ) =
                    if i == item && iq >= qty then
                        ( i, iq - qty )
                        --TODO spend gp for buying

                    else
                        ( i, iq )

                updated_inv_items =
                    List.map (\( i, q ) -> ( i, q + qty )) <|
                        List.filter (find_matching_records item) model.owned_items

                non_matching_inv_items =
                    List.filter (not << find_matching_records item) model.owned_items

                new_inventory =
                    case List.length updated_inv_items of
                        0 ->
                            model.owned_items ++ [ ( item, qty ) ]

                        _ ->
                            non_matching_inv_items ++ updated_inv_items

                new_shop_items =
                    List.map reduce_if_matched model.items_for_sale

                new_model =
                    if can_afford_item then
                        { model
                            | owned_items = new_inventory
                            , items_for_sale = new_shop_items
                            , gold_in_pocket = model.gold_in_pocket - total_cost
                        }

                    else
                        model
            in
            ( new_model, Cmd.none )

        SellItem item qty ->
            let
                reduce_if_matched ( i, iq ) =
                    if i == item && iq >= qty then
                        ( i, iq - qty )
                        --TODO get gp for selling

                    else
                        ( i, iq )

                has_items_to_sell =
                    List.length
                        (List.filter
                            (\( i, q ) ->
                                q >= qty && find_matching_records item ( i, q )
                            )
                            model.owned_items
                        )
                        > 0

                updated_shop_items =
                    List.map (\( i, q ) -> ( i, q + qty )) <|
                        List.filter (find_matching_records item) model.items_for_sale

                non_matching_shop_items =
                    List.filter (not << find_matching_records item) model.items_for_sale

                new_shop_items =
                    case List.length updated_shop_items of
                        0 ->
                            model.items_for_sale ++ [ ( item, qty ) ]

                        _ ->
                            non_matching_shop_items ++ updated_shop_items

                new_inventory =
                    List.map reduce_if_matched model.owned_items

                new_model =
                    if has_items_to_sell then
                        { model
                            | owned_items = new_inventory
                            , items_for_sale = new_shop_items
                            , gold_in_pocket = model.gold_in_pocket + get_adjusted_item_cost item qty
                        }

                    else
                        model
            in
            ( new_model, Cmd.none )


render_item_type : ItemType -> Element.Element Msg
render_item_type item_type =
    text <| item_type_to_pretty_string item_type


clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


font_grey : Element.Attribute msg
font_grey =
    Font.color <| rgb 0.35 0.35 0.35


render_gp count =
    render_gp_sized count 12


render_gp_sized count font_size =
    paragraph []
        [ text <| String.fromInt count
        , Element.el [ Font.size font_size, font_grey ] (text "gp")
        ]


shop_buy_button gold_in_pocket ( item, qty ) =
    let
        can_afford =
            gold_in_pocket >= get_adjusted_item_cost item 1

        button_type =
            if can_afford then
                primary_button

            else
                secondary_button
    in
    button_type
        [ Element.transparent <| qty < 1, width (fill |> Element.minimum 120) ]
        (BuyItem item 1)
    <|
        if can_afford then
            "buy me"

        else
            "can't afford"


debug_explain =
    let
        do_explain =
            False
    in
    if do_explain then
        explain Debug.todo

    else
        Element.scale 1.0


render_single_item_for_sale : Int -> Maybe Item -> ( Item, Int ) -> ListContext -> Element.Element Msg
render_single_item_for_sale gold_in_pocket maybe_hovered_item ( item, qty ) context =
    let
        is_hovered_item =
            case maybe_hovered_item of
                Just hovered_item ->
                    item == hovered_item

                Nothing ->
                    False

        --shown when hovered over item
        expanded_display =
            case is_hovered_item of
                True ->
                    Element.el
                        [ width fill
                        , Background.color <| rgb 1 1 1
                        , Border.color <| rgb 0.35 0.35 0.35
                        , Border.rounded 3
                        , Border.width 2
                        , padding 10
                        , Element.moveDown 20
                        ]
                    <|
                        paragraph []
                            [ text item.name
                            , text ": "
                            , text item.description
                            ]

                False ->
                    Element.none

        controls_column =
            case context of
                ShopItems ->
                    shop_buy_button gold_in_pocket ( item, qty )

                InventoryItems ->
                    primary_button
                        [ Element.transparent <| qty < 1 ]
                        (SellItem item 1)
                        "sell me"
    in
    row
        [ font_scaled 1
        , width fill
        , spacingXY 5 0

        -- , Element.spaceEvenly
        , Events.onMouseEnter <| MouseEnterShopItem context item
        , Events.onMouseLeave <| MouseLeaveShopItem context item
        , Element.below expanded_display
        ]
        [ column [ width (fillPortion 2 |> Element.maximum 200), font_scaled 2, debug_explain ] [ text <| clipText item.name 15 ]
        , column [ portion 1, debug_explain ] [ render_gp <| get_adjusted_item_cost item 1 ]
        , column [ portion 2, debug_explain ] [ render_item_type item.item_type ]
        , column [ portion 1, debug_explain ]
            [ el [] <|
                if qty == 1 then
                    text " "

                else if qty == 0 then
                    text <|
                        case context of
                            ShopItems ->
                                "SOLD OUT"

                            InventoryItems ->
                                "NONE LEFT"

                else
                    text <| "x" ++ String.fromInt qty
            ]
        , column [ width <| (fillPortion 3 |> Element.maximum 200), debug_explain ]
            [ el [] <| text <| clipText item.description 24 ]
        , column [ portion 1, debug_explain ] [ controls_column ]
        ]


portion : Int -> Element.Attribute msg
portion =
    width << fillPortion


padding_bottom : Int -> Element.Attribute msg
padding_bottom pad =
    Element.paddingEach { bottom = pad, left = 0, right = 0, top = 0 }


border_bottom : Int -> Element.Attribute msg
border_bottom bord =
    Border.widthEach { bottom = bord, left = 0, right = 0, top = 0 }


view : Model -> Html.Html Msg
view model =
    let
        welcome_header =
            Element.el [ font_scaled 3, padding_bottom 10 ] <| text "Welcome to the Item Shop!"

        sort_func =
            Tuple.first >> .name

        -- items_for_sale_table =
        --     Element.table [ width fill, font_scaled 1, spacing 5, paddingXY 0 10 ]
        --         { data = List.sortBy sort_func model.items_for_sale
        --         , columns =
        --             [ { header = Element.none
        --               , view = Tuple.first >> .name >> text >> el [ font_scaled 2, debug_explain ]
        --               , width = fillPortion 2
        --               }
        --             , { header = Element.none
        --               , view = Tuple.first >> .gold_cost >> render_gp >> el [ debug_explain ]
        --               , width = fillPortion 1
        --               }
        --             , { header = Element.none
        --               , view = Tuple.first >> .item_type >> render_item_type >> el [ debug_explain ]
        --               , width = fillPortion 2
        --               }
        --             , { header = Element.none
        --               , view =
        --                     (\( i, qty ) ->
        --                         if qty == 1 then
        --                             text ""
        --
        --                         else if qty == 0 then
        --                             text "SOLD OUT"
        --
        --                         else
        --                             text <| "x" ++ String.fromInt qty
        --                     )
        --                         >> el [ debug_explain ]
        --               , width = fillPortion 1
        --               }
        --             , { header = Element.none
        --               , view = Tuple.first >> .description >> (\desc -> clipText desc 24) >> text >> el [ alignLeft, debug_explain ]
        --               , width = fillPortion 2
        --               }
        --             , { header = Element.none
        --               , view = \( i, q ) -> el [ debug_explain ] <| shop_buy_button model.gold_in_pocket ( i, q )
        --               , width = fillPortion 1
        --               }
        --             ]
        --         }
        items_for_sale_grid =
            Element.column [ width fill, spacingXY 0 5 ] <|
                (++)
                    [ Element.el [ border_bottom 2 ] <| text "Items For Sale"
                    ]
                    (List.map
                        (\item ->
                            render_single_item_for_sale
                                model.gold_in_pocket
                                model.hovered_item_for_sale
                                item
                                ShopItems
                        )
                     <|
                        List.sortBy sort_func model.items_for_sale
                    )

        items_in_inventory =
            Element.column [ width fill, spacingXY 0 5 ] <|
                (++)
                    [ Element.row [ width fill ]
                        [ Element.el [ border_bottom 2 ] <| text "Items In Inventory"
                        , text "   "
                        , row [ font_scaled 1, centerX ] <|
                            [ text "Held: ", render_gp model.gold_in_pocket ]
                        ]
                    ]
                    (List.map
                        (\item ->
                            render_single_item_for_sale
                                model.gold_in_pocket
                                model.hovered_item_in_inventory
                                item
                                InventoryItems
                        )
                     <|
                        List.sortBy sort_func model.owned_items
                    )
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        Element.column [ width fill ]
            [ welcome_header

            -- , el [ font_scaled 2, border_bottom 2 ] <| text "Items for Sale"
            -- , items_for_sale_table
            , items_for_sale_grid
            , Element.el [ paddingXY 0 10, width fill ] items_in_inventory
            ]


scaled : Int -> Int
scaled val =
    modular 16 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale
