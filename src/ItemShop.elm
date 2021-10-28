module ItemShop exposing (Model, Msg, init, subscriptions, update, view)

import Array
import Color
import Color.Convert as Convert
import Dict
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
import Random
import Task
import Time
import UUID exposing (UUID)


type ItemType
    = Weapon
    | Armor
    | Spellbook
    | Furniture
    | Food


type alias ItemTypeId =
    Int


type alias ItemId =
    UUID


type alias CharacterId =
    UUID


item_type_to_id : ItemType -> ItemTypeId
item_type_to_id item_type =
    case item_type of
        Weapon ->
            1

        Armor ->
            2

        Spellbook ->
            3

        Furniture ->
            4

        Food ->
            5


id_to_item_type : ItemTypeId -> Maybe ItemType
id_to_item_type type_id =
    case type_id of
        1 ->
            Just Weapon

        2 ->
            Just Armor

        3 ->
            Just Spellbook

        4 ->
            Just Furniture

        5 ->
            Just Food

        _ ->
            Nothing


type alias Item =
    { name : String
    , item_type : ItemType
    , raw_gold_cost : Int
    , description : String
    , id : ItemId
    }


type ListContext
    = ShopItems
    | InventoryItems
    | CharacterItems


type TradeParty
    = ShopParty
    | PlayerParty
    | CharacterParty CharacterId


trade_party_to_str : TradeParty -> String
trade_party_to_str party =
    case party of
        ShopParty ->
            "Shop"

        PlayerParty ->
            "Player"

        CharacterParty char_id ->
            "CharacterId"


type Msg
    = Noop
    | MouseEnterShopItem ListContext Item
    | MouseLeaveShopItem ListContext Item
    | BuyItem Item Int
    | SellItem Item Int
    | StartTrendsHover
    | EndTrendsHover
    | UpdateTick Time.Posix


type alias InventoryRecord =
    ( Item, Int )


type alias InventoryRecords =
    List InventoryRecord


type alias ItemSentiments =
    Dict.Dict ItemTypeId Float


type alias ShopTrends =
    { item_type_sentiment : ItemSentiments
    , item_trade_logs : List ItemTradeLog
    }


type alias ItemTradeLog =
    { item_id : ItemId
    , quantity : Int
    , gold_cost : Int
    , from_party : TradeParty
    , to_party : TradeParty
    }


type alias Character =
    { held_items : InventoryRecords
    , held_gold : Int
    , char_id : CharacterId
    , name : String
    }


type alias Model =
    { owned_items : InventoryRecords
    , items_for_sale : InventoryRecords
    , character : Character
    , hovered_item_for_sale : Maybe Item
    , hovered_item_in_inventory : Maybe Item
    , hovered_item_in_character : Maybe Item
    , gold_in_pocket : Int
    , shop_trends : ShopTrends
    , shop_trends_hovered : Bool
    }


initial_shop_trends : ShopTrends
initial_shop_trends =
    { item_type_sentiment =
        Dict.fromList <|
            List.map (\it -> ( item_type_to_id it, 1.0 ))
                [ Weapon
                , Armor
                , Spellbook
                , Furniture
                , Food
                ]
    , item_trade_logs = []
    }


unset_item_frame : Item
unset_item_frame =
    { name = "UNSET ITEM"
    , item_type = Armor
    , raw_gold_cost = 5
    , description = "An unset item description for debugging"
    , id = UUID.forName "unset item " UUID.dnsNamespace
    }


item_frames =
    Dict.fromList
        [ ( "boots"
          , { name = "Boots"
            , item_type = Armor
            , raw_gold_cost = 5
            , description = "An old pair of boots"
            , id = UUID.forName "boots" UUID.dnsNamespace
            }
          )
        , ( "sword"
          , { name = "Sword"
            , item_type = Weapon
            , raw_gold_cost = 15
            , description = "A rusted sword"
            , id = UUID.forName "rusted sword" UUID.dnsNamespace
            }
          )
        , ( "dagger"
          , { name = "Dagger"
            , item_type = Weapon
            , raw_gold_cost = 10
            , description = "A small weapon that fits in your pocket"
            , id = UUID.forName "small dagger" UUID.dnsNamespace
            }
          )
        , ( "book"
          , { name = "Book of the Dead with a real"
            , item_type = Spellbook
            , raw_gold_cost = 20
            , description = "Bound in leather, this book has a skull for a cover"
            , id = UUID.forName "book of the dead" UUID.dnsNamespace
            }
          )
        ]


get_item_by_str : String -> Maybe Item
get_item_by_str item_str =
    Dict.get item_str item_frames


get_item_by_id : UUID -> Maybe Item
get_item_by_id item_id =
    List.head <|
        List.filter
            (\item -> item.id == item_id)
        <|
            List.map Tuple.second <|
                Dict.toList item_frames


initial_items_for_sale : List ( Item, Int )
initial_items_for_sale =
    let
        seed =
            Random.initialSeed 12345

        maybe_items : List ( String, Int )
        maybe_items =
            [ ( "boots", 3 )
            , ( "sword", 5 )
            , ( "dagger", 4 )
            , ( "book", 1 )
            ]

        just_items : List ( Item, Int )
        just_items =
            List.filterMap
                (\( item_str, qty ) ->
                    case Dict.get item_str item_frames of
                        Just item ->
                            Just ( item, qty )

                        Nothing ->
                            Nothing
                )
                maybe_items

        debug =
            if List.length just_items == List.length maybe_items then
                Ok ""

            else
                --TODO: handling this better, because this isn't elmy at all
                Debug.log "THERE WAS AN ERROR IN INITIAL ITEM SETUP!!!!" <| Err ""
    in
    just_items


initial_owned_items : List ( Item, Int )
initial_owned_items =
    [ ( Maybe.withDefault unset_item_frame <| Dict.get "boots" item_frames, 12 )
    ]


initial_character : Character
initial_character =
    { held_items =
        [ ( Maybe.withDefault unset_item_frame <| Dict.get "dagger" item_frames, 8 )
        ]
    , held_gold = 100
    , char_id = UUID.forName "character 1" UUID.dnsNamespace
    , name = "Billy"
    }


get_adjusted_item_cost : ShopTrends -> Item -> Int -> Int
get_adjusted_item_cost shop_trends item qty =
    let
        { item_type } =
            item

        item_sentiment =
            case
                Dict.get
                    (item_type_to_id item_type)
                    shop_trends.item_type_sentiment
            of
                Just pop ->
                    pop

                Nothing ->
                    1.0

        scaled_raw_cost =
            toFloat <| item.raw_gold_cost * qty
    in
    round <| scaled_raw_cost * item_sentiment


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


item_type_to_pretty_string_plural : ItemType -> String
item_type_to_pretty_string_plural item_type =
    case item_type of
        Weapon ->
            "Weapons"

        Armor ->
            "Armors"

        Spellbook ->
            "Spellbooks"

        Furniture ->
            "Furniture"

        Food ->
            "Food"


init : ( Model, Cmd Msg )
init =
    ( { owned_items = initial_owned_items
      , items_for_sale = initial_items_for_sale
      , character = initial_character
      , hovered_item_for_sale = Nothing
      , hovered_item_in_inventory = Nothing
      , hovered_item_in_character = Nothing
      , gold_in_pocket = 0
      , shop_trends = initial_shop_trends
      , shop_trends_hovered = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 UpdateTick


find_matching_records : Item -> InventoryRecord -> Bool
find_matching_records to_match ( recorded_item, _ ) =
    to_match == recorded_item


can_afford_item : ShopTrends -> Int -> Item -> Int -> Bool
can_afford_item shop_trends held_gold item qty =
    held_gold >= get_adjusted_item_cost shop_trends item qty


add_item_to_inventory_records : InventoryRecords -> Item -> Int -> InventoryRecords
add_item_to_inventory_records records item qty =
    let
        updated_inv_items =
            List.map (\( i, q ) -> ( i, q + qty )) <|
                List.filter (find_matching_records item) records

        non_matching_inv_items =
            List.filter (not << find_matching_records item) records
    in
    case List.length updated_inv_items of
        0 ->
            records ++ [ ( item, qty ) ]

        _ ->
            non_matching_inv_items ++ updated_inv_items


reduce_if_matched item qty ( i, iq ) =
    if i == item && iq >= qty then
        ( i, iq - qty )
        --TODO spend gp for buying

    else
        ( i, iq )


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

                CharacterItems ->
                    ( { model | hovered_item_in_character = Just item }, Cmd.none )

        MouseLeaveShopItem context item ->
            case context of
                ShopItems ->
                    ( { model | hovered_item_for_sale = Nothing }, Cmd.none )

                InventoryItems ->
                    ( { model | hovered_item_in_inventory = Nothing }, Cmd.none )

                CharacterItems ->
                    ( { model | hovered_item_in_character = Nothing }, Cmd.none )

        BuyItem item qty ->
            let
                total_cost =
                    get_adjusted_item_cost model.shop_trends item qty

                can_afford_item_ =
                    can_afford_item model.shop_trends model.gold_in_pocket item qty

                new_inventory =
                    add_item_to_inventory_records model.owned_items item qty

                new_shop_items =
                    List.map  (reduce_if_matched item qty) model.items_for_sale

                { shop_trends } =
                    model

                { item_type_sentiment } =
                    shop_trends

                new_its =
                    Dict.update
                        (item_type_to_id item.item_type)
                        (\maybe_sent ->
                            case maybe_sent of
                                Just existing_sent ->
                                    Just (existing_sent + 0.1)

                                Nothing ->
                                    Just (1.0 + 0.1)
                        )
                        item_type_sentiment

                from_party : TradeParty
                from_party =
                    ShopParty

                to_party : TradeParty
                to_party =
                    PlayerParty

                old_trade_logs =
                    shop_trends.item_trade_logs

                new_trade_log_entry : ItemTradeLog
                new_trade_log_entry =
                    { item_id = item.id
                    , quantity = qty
                    , gold_cost = total_cost
                    , from_party = from_party
                    , to_party = to_party
                    }

                new_item_trade_logs =
                    old_trade_logs
                        ++ [ new_trade_log_entry ]

                new_shop_trends =
                    { shop_trends
                        | item_type_sentiment = new_its
                        , item_trade_logs = new_item_trade_logs
                    }

                new_model =
                    if can_afford_item_ then
                        { model
                            | owned_items = new_inventory
                            , items_for_sale = new_shop_items
                            , gold_in_pocket = model.gold_in_pocket - total_cost
                            , shop_trends = new_shop_trends
                        }

                    else
                        model
            in
            ( new_model, Cmd.none )

        SellItem item qty ->
            let
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
                    List.map (reduce_if_matched item qty) model.owned_items

                { shop_trends } =
                    model

                { item_type_sentiment } =
                    shop_trends

                new_its =
                    Dict.update
                        (item_type_to_id item.item_type)
                        (\maybe_sent ->
                            case maybe_sent of
                                Just existing_sent ->
                                    Just (existing_sent - 0.1)

                                Nothing ->
                                    Just (1.0 - 0.1)
                        )
                        item_type_sentiment

                from_party : TradeParty
                from_party =
                    PlayerParty

                to_party : TradeParty
                to_party =
                    ShopParty

                old_trade_logs =
                    shop_trends.item_trade_logs

                new_trade_log_entry : ItemTradeLog
                new_trade_log_entry =
                    { item_id = item.id
                    , quantity = qty
                    , gold_cost = total_cost
                    , from_party = from_party
                    , to_party = to_party
                    }

                new_item_trade_logs =
                    old_trade_logs
                        ++ [ new_trade_log_entry ]

                new_shop_trends =
                    { shop_trends
                        | item_type_sentiment = new_its
                        , item_trade_logs = new_item_trade_logs
                    }

                total_cost =
                    get_adjusted_item_cost model.shop_trends item qty

                new_model =
                    if has_items_to_sell then
                        { model
                            | owned_items = new_inventory
                            , items_for_sale = new_shop_items
                            , gold_in_pocket = model.gold_in_pocket + total_cost
                            , shop_trends = new_shop_trends
                        }

                    else
                        model
            in
            ( new_model, Cmd.none )

        StartTrendsHover ->
            ( { model | shop_trends_hovered = True }, Cmd.none )

        EndTrendsHover ->
            ( { model | shop_trends_hovered = False }, Cmd.none )

        UpdateTick time ->
            let
                _ =
                    Debug.log "tick" time
            in
            ( model, Cmd.none )


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


render_gp : Int -> Element msg
render_gp count =
    render_gp_sized count 12


render_gp_sized : Int -> Int -> Element msg
render_gp_sized count font_size =
    paragraph []
        [ text <| String.fromInt count
        , Element.el [ Font.size font_size, font_grey ] (text "gp")
        ]


shop_buy_button : Int -> Int -> InventoryRecord -> Element Msg
shop_buy_button gold_cost gold_in_pocket ( item, qty ) =
    let
        can_afford =
            gold_in_pocket >= gold_cost

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


is_item_trending : ItemSentiments -> Item -> Bool
is_item_trending item_type_sentiments item =
    case Dict.get (item_type_to_id item.item_type) item_type_sentiments of
        Just existing_sent ->
            existing_sent /= 1.0

        Nothing ->
            False


render_single_item_for_sale : ShopTrends -> Int -> Maybe Item -> ( Item, Int ) -> ListContext -> Element.Element Msg
render_single_item_for_sale shop_trends gold_in_pocket maybe_hovered_item ( item, qty ) context =
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
                        column [ spacing 5 ]
                            [ paragraph []
                                [ text item.name
                                , text ": "
                                , text item.description
                                ]
                            , paragraph [] <|
                                [ text "Current Price: "
                                , render_gp current_price
                                ]
                                    ++ (if
                                            is_item_trending
                                                shop_trends.item_type_sentiment
                                                item
                                                && item.raw_gold_cost
                                                /= current_price
                                        then
                                            [ text " (originally "
                                            , render_gp item.raw_gold_cost
                                            , text ")"
                                            ]

                                        else
                                            []
                                       )
                            ]

                False ->
                    Element.none

        current_price =
            get_adjusted_item_cost shop_trends item 1

        controls_column =
            case context of
                ShopItems ->
                    shop_buy_button current_price gold_in_pocket ( item, qty )

                InventoryItems ->
                    primary_button
                        [ Element.transparent <| qty < 1 ]
                        (SellItem item 1)
                        "sell me"

                CharacterItems ->
                    Element.none
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
        , column [ portion 1, debug_explain ] [ render_gp <| get_adjusted_item_cost shop_trends item 1 ]
        , column [ portion 2, debug_explain ] [ render_item_type item.item_type ]
        , column [ portion 1, debug_explain ]
            [ el [] <|
                if qty == 1 then
                    text " "

                else if qty == 0 then
                    case context of
                        ShopItems ->
                            text "SOLD OUT"

                        InventoryItems ->
                            text "NONE LEFT"

                        CharacterItems ->
                            Element.none

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


render_single_trade_log_entry : ItemTradeLog -> Element msg
render_single_trade_log_entry trade_log =
    let
        { from_party, to_party, item_id, quantity, gold_cost } =
            trade_log

        maybe_item =
            get_item_by_id item_id

        qty_str =
            String.fromInt quantity |> (++) "x"

        rendered_cost : Element msg
        rendered_cost =
            render_gp gold_cost

        item_name =
            case maybe_item of
                Just item ->
                    item.name

                Nothing ->
                    "Unknown item"
    in
    case ( from_party, to_party ) of
        ( ShopParty, PlayerParty ) ->
            paragraph []
                [ text <|
                    item_name
                        ++ " ("
                        ++ qty_str
                        ++ ") "
                        ++ "was purchased for "
                , rendered_cost
                ]

        ( PlayerParty, ShopParty ) ->
            paragraph []
                [ text <|
                    item_name
                        ++ " ("
                        ++ qty_str
                        ++ ") "
                        ++ "was sold for "
                , rendered_cost
                ]

        _ ->
            text <|
                "an item was traded from "
                    ++ trade_party_to_str from_party
                    ++ " to "
                    ++ trade_party_to_str to_party


trends_display : ShopTrends -> Bool -> Element.Element Msg
trends_display shop_trends is_expanded =
    let
        render_single_popularity : ( Int, Float ) -> Element.Element msg
        render_single_popularity ( type_id, popularity ) =
            let
                pretty_type : String
                pretty_type =
                    case id_to_item_type type_id of
                        Just prettied ->
                            item_type_to_pretty_string_plural prettied

                        Nothing ->
                            "Unknown Type (" ++ String.fromInt type_id ++ ")"
            in
            text <| pretty_type ++ ": " ++ String.fromInt (round (popularity * 100)) ++ "%"

        has_active_trends =
            List.any (Tuple.second >> (/=) 1) <| Dict.toList shop_trends.item_type_sentiment

        summarized : Element msg
        summarized =
            if not has_active_trends then
                text "No trends at the moment."

            else
                text "Something is trending!"

        specific_trends : List (Element msg)
        specific_trends =
            List.map render_single_popularity <|
                List.filter (Tuple.second >> (/=) 1) <|
                    Dict.toList shop_trends.item_type_sentiment

        rendered_item_trade_logs : List (Element msg)
        rendered_item_trade_logs =
            [ text <|
                "Trade logs go here, and there have been "
                    ++ (String.fromInt <| List.length shop_trends.item_trade_logs)
                    ++ " trades"
            ]

        rendered_popularity : Element.Element msg
        rendered_popularity =
            row [ spacing 15, paddingXY 0 10 ] <|
                summarized
                    :: specific_trends
                    ++ rendered_item_trade_logs

        expanded_trade_logs =
            Element.below <|
                column
                    [ width fill
                    , Background.color <| rgb 1 1 1
                    , Border.color <| rgb 0.35 0.35 0.35
                    , Border.rounded 3
                    , Border.width 2
                    , padding 10
                    , spacing 5
                    , Element.moveDown 20
                    ]
                <|
                    List.map
                        render_single_trade_log_entry
                        shop_trends.item_trade_logs

        has_trades =
            List.length shop_trends.item_trade_logs > 0
    in
    column
        ([ paddingXY 0 5, width fill ]
            ++ (if has_trades then
                    [ Events.onMouseEnter <| StartTrendsHover
                    , Events.onMouseLeave <| EndTrendsHover
                    ]

                else
                    []
               )
            ++ (if is_expanded then
                    [ expanded_trade_logs ]

                else
                    []
               )
        )
    <|
        [ el [ font_scaled 2, border_bottom 2 ] <| text "Shop Trends"
        , rendered_popularity
        ]


render_inventory :
    String
    -> InventoryRecords
    -> Int
    -> ShopTrends
    -> Maybe Item
    -> ListContext
    -> Element Msg
render_inventory header held_items gold_held shop_trends hovered_item context =
    Element.column [ width fill, spacingXY 0 5 ] <|
        (++)
            [ Element.row [ font_scaled 2, width fill ]
                [ Element.el [ border_bottom 2 ] <| text header
                , text "   "
                , row [ font_scaled 1, centerX ] <|
                    [ text "Held: ", render_gp gold_held ]
                ]
            ]
            (List.map
                (\item ->
                    render_single_item_for_sale
                        shop_trends
                        gold_held
                        hovered_item
                        item
                        context
                )
             <|
                List.sortBy sort_func held_items
            )


sort_func =
    Tuple.first >> .name


view : Model -> Html.Html Msg
view model =
    let
        welcome_header =
            Element.el [ font_scaled 3, padding_bottom 10 ] <| text "Welcome to the Item Shop!"

        items_for_sale_grid =
            Element.column [ width fill, spacingXY 0 5 ] <|
                (++)
                    [ Element.el [ font_scaled 2, border_bottom 2 ] <| text "Items For Sale"
                    ]
                    (List.map
                        (\item ->
                            render_single_item_for_sale
                                model.shop_trends
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
                    [ Element.row [ font_scaled 2, width fill ]
                        [ Element.el [ border_bottom 2 ] <| text "Items In Inventory"
                        , text "   "
                        , row [ font_scaled 1, centerX ] <|
                            [ text "Held: ", render_gp model.gold_in_pocket ]
                        ]
                    ]
                    (List.map
                        (\item ->
                            render_single_item_for_sale
                                model.shop_trends
                                model.gold_in_pocket
                                model.hovered_item_in_inventory
                                item
                                InventoryItems
                        )
                     <|
                        List.sortBy sort_func model.owned_items
                    )

        character =
            model.character
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        Element.column [ width fill, font_scaled 1 ]
            [ welcome_header
            , trends_display model.shop_trends model.shop_trends_hovered

            -- , el [ font_scaled 2, border_bottom 2 ] <| text "Items for Sale"
            -- , items_for_sale_table
            , items_for_sale_grid
            , Element.el [ paddingXY 0 10, width fill ] items_in_inventory
            , Element.el [ paddingXY 0 10, width fill ]
                (render_inventory
                    (character.name ++ "'s Inventory")
                    model.character.held_items
                    model.character.held_gold
                    model.shop_trends
                    model.hovered_item_in_character
                    CharacterItems
                )
            ]


scaled : Int -> Int
scaled val =
    modular 16 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale
