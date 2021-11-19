module ItemShop exposing (Model, Msg, init, subscriptions, update, view)

import Array
import Browser.Events
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
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
        , scrollbars
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
import Html.Events
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Random
import Random.List
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


type alias ItemIdStr =
    String


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


trade_party_to_str : List Character -> TradeParty -> String
trade_party_to_str all_characters party =
    case party of
        ShopParty ->
            "Shop"

        PlayerParty ->
            "Player"

        CharacterParty char_id ->
            case List.head <| List.filter (.char_id >> (==) char_id) all_characters of
                Just character ->
                    character.name

                Nothing ->
                    "Unnamed Character"


type alias TrendChartDatum =
    ( Int, ( ItemType, Float ) )


type KeyEventMsg
    = KeyEventControl
    | KeyEventAlt
    | KeyEventShift
    | KeyEventMeta
    | KeyEventLetter Char
    | KeyEventUnknown String


type Msg
    = Noop
    | MouseEnterShopItem ListContext ( CharacterId, Item )
    | MouseLeaveShopItem ListContext ( CharacterId, Item )
    | BuyItem Item Int
    | SellItem Item Int
    | StartTrendsHover
    | EndTrendsHover
    | TickSecond Time.Posix
      -- | OnTrendChartHover (List (CI.One TrendSnapshot CI.Dot))
    | OnTrendChartHover (List (CI.One TrendChartDatum CI.Dot))
    | ToggleShowDebugInventories
    | KeyPressedMsg KeyEventMsg
    | KeyReleasedMsg KeyEventMsg


type alias TradeOrder =
    { item : Item
    , qty : Int
    }


type alias InventoryRecord =
    ( Item, Int )


type alias InventoryRecords =
    List InventoryRecord


type alias ItemTypeIdSentiment =
    ( ItemTypeId, Float )


type alias ItemTypeSentiment =
    ( ItemType, Float )


type alias ItemSentiments =
    Dict.Dict ItemTypeId Float


type alias ShopTrends =
    { item_type_sentiment : ItemSentiments
    , item_trade_logs : List ItemTradeLog
    }


type WantedAction
    = WantedToBuy
    | WantedToSell


type ActionLogType
    = Traded ItemTradeLog
    | WantedButCouldntTrade WantedAction
    | DidNothing


type alias ActionLog =
    { log_type : ActionLogType
    , time : Time.Posix
    }


type alias ItemTradeLog =
    { item_id : ItemId
    , quantity : Int
    , gold_cost : Int
    , from_party : TradeParty
    , to_party : TradeParty
    }


type alias TrendTolerance =
    { --highest it'll buy the item type for
      buy : ItemSentiments

    --lowest it'll sell the item type for
    , sell : ItemSentiments
    }


type alias Character =
    { held_items : InventoryRecords
    , held_gold : Int
    , char_id : CharacterId
    , name : String
    , party : TradeParty
    , trend_tolerance : TrendTolerance
    , item_types_desired : ItemSentiments
    , action_log : List ActionLog
    }


type alias TrendSnapshot =
    { time : Time.Posix, item_type : ItemType, value : Float }


type alias ItemDb =
    Dict.Dict ItemIdStr Item


type alias Model =
    { player : Character
    , shop : Character
    , characters : List Character
    , hovered_item_in_character : Maybe ( CharacterId, Item )
    , shop_trends : ShopTrends
    , historical_shop_trends : List ShopTrends
    , shop_trends_hovered : Bool
    , item_db : ItemDb
    , hovered_trend_chart : List (CI.One TrendChartDatum CI.Dot)
    , ai_tick_time : Time.Posix --used to seed the ai randomness
    , show_debug_inventories : Bool
    , show_charts_in_hovered_item : Bool
    }


is_item_trade_log_to_shop : ItemTradeLog -> Bool
is_item_trade_log_to_shop item_trade_log =
    item_trade_log.to_party == ShopParty


is_item_trade_log_from_shop : ItemTradeLog -> Bool
is_item_trade_log_from_shop item_trade_log =
    item_trade_log.from_party == ShopParty


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


initial_item_db : Dict.Dict String Item
initial_item_db =
    let
        initial_items =
            [ { name = "Boots"
              , item_type = Armor
              , raw_gold_cost = 5
              , description = "An old pair of boots"
              , id = UUID.forName "boots" UUID.dnsNamespace
              }
            , { name = "Sword"
              , item_type = Weapon
              , raw_gold_cost = 15
              , description = "A rusted sword"
              , id = UUID.forName "rusted sword" UUID.dnsNamespace
              }
            , { name = "Dagger"
              , item_type = Weapon
              , raw_gold_cost = 10
              , description = "A small weapon that fits in your pocket"
              , id = UUID.forName "small dagger" UUID.dnsNamespace
              }
            , { name = "Book of the Dead"
              , item_type = Spellbook
              , raw_gold_cost = 20
              , description = "Bound in leather, this book has a skull for a cover"
              , id = UUID.forName "book of the dead" UUID.dnsNamespace
              }
            ]
    in
    Dict.fromList <|
        List.map
            (\item ->
                ( UUID.toString item.id, item )
            )
            initial_items



-- get_item_by_str : String -> Maybe Item
-- get_item_by_str item_str =
--     Dict.get item_str item_frames
-- get_item_by_id : UUID -> Maybe Item
-- get_item_by_id item_id =
--     List.head <|
--         List.filter
--             (\item -> item.id == item_id)
--         <|
--             List.map Tuple.second <|
--                 Dict.toList item_frames


initial_items_for_sale : ItemDb -> InventoryRecords
initial_items_for_sale item_db =
    let
        seed =
            Random.initialSeed 12345

        maybe_items : List ( String, Int )
        maybe_items =
            [ ( "a41ae9d3-61f0-54f9-800e-56f53ed3ac98", 3 ) --boots
            , ( "c3c38323-1743-5a47-a8e3-bf6ec28137f9", 5 )
            , ( "6b7e301d-ab12-5e81-acfc-547e63004ffa", 4 )
            , ( "48e66792-4c97-598f-8937-3a7042f00591", 1 )
            ]

        just_items : List ( Item, Int )
        just_items =
            List.filterMap
                (\( item_id_str, qty ) ->
                    case lookup_item_id_str item_db item_id_str of
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


initial_owned_items : ItemDb -> InventoryRecords
initial_owned_items item_db =
    [ ( lookup_item_id_str_default item_db "a41ae9d3-61f0-54f9-800e-56f53ed3ac98", 12 )
    ]


initial_characters : ItemDb -> List Character
initial_characters item_db =
    let
        base_character_1 =
            create_character (UUID.forName "character 1" UUID.dnsNamespace) "Billy"

        base_character_2 =
            create_character (UUID.forName "character 2" UUID.dnsNamespace) "Mitchell"
    in
    [ { base_character_1
        | held_items =
            [ ( lookup_item_id_str_default item_db "6b7e301d-ab12-5e81-acfc-547e63004ffa", 8 )
            ]
        , held_gold = 100
        , item_types_desired = Dict.fromList [ ( item_type_to_id Weapon, 0.0 ) ]
      }
    , { base_character_2
        | held_items =
            [ ( lookup_item_id_str_default item_db "a41ae9d3-61f0-54f9-800e-56f53ed3ac98", 12 )
            ]
        , held_gold = 200
        , item_types_desired = Dict.fromList [ ( item_type_to_id Spellbook, 0.0 ) ]
      }
    ]


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


primary_color_bright : Color
primary_color_bright =
    case Convert.hexToColor "#66b0ff" of
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
         , Element.mouseOver
            [ Background.color <| primary_color_bright
            , Border.color <| primary_color_bright
            , Font.color <| rgb 0 0 0
            ]
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



-- this doesn't help me so far


scrollbarYEl : List (Element.Attribute msg) -> Element msg -> Element msg
scrollbarYEl attrs body =
    el [ height fill, width fill ] <|
        el
            ([ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
             , Element.htmlAttribute <| Html.Attributes.style "top" "0"
             , Element.htmlAttribute <| Html.Attributes.style "right" "0"
             , Element.htmlAttribute <| Html.Attributes.style "bottom" "0"
             , Element.htmlAttribute <| Html.Attributes.style "left" "0"
             , Element.scrollbarY
             ]
                ++ attrs
            )
            body


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


empty_item_sentiments : ItemSentiments
empty_item_sentiments =
    Dict.empty


empty_trend_tolerance : TrendTolerance
empty_trend_tolerance =
    { buy = Dict.empty, sell = Dict.empty }


create_character : UUID -> String -> Character
create_character char_id name =
    { held_items = []
    , held_gold = 0

    -- , char_id = UUID.forName "player character" UUID.dnsNamespace
    , char_id = char_id

    -- , name = "Player"
    , name = name
    , party = CharacterParty char_id
    , trend_tolerance = empty_trend_tolerance
    , item_types_desired = empty_item_sentiments
    , action_log = []
    }


init : ( Model, Cmd Msg )
init =
    let
        player_base_char =
            create_character (UUID.forName "player character" UUID.dnsNamespace) "Player"

        shop_base_char =
            create_character (UUID.forName "shop character" UUID.dnsNamespace) "Shop"

        item_db =
            initial_item_db

        player : Character
        player =
            { player_base_char
                | held_items = initial_owned_items item_db
                , held_gold = 0
                , party = PlayerParty
            }

        shop : Character
        shop =
            { shop_base_char
                | held_items = initial_items_for_sale item_db
                , held_gold = 999999999
                , party = ShopParty
            }

        characters : List Character
        characters =
            [ player, shop ] ++ initial_characters item_db
    in
    ( { player = player
      , shop = shop
      , characters = characters
      , hovered_item_in_character = Nothing
      , shop_trends = initial_shop_trends
      , item_db = item_db
      , historical_shop_trends = []
      , shop_trends_hovered = False
      , ai_tick_time = Time.millisToPosix -1
      , hovered_trend_chart = []
      , show_debug_inventories = True
      , show_charts_in_hovered_item = False
      }
    , Task.perform TickSecond Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Sub.batch
        [ Time.every 1000 TickSecond
        , Browser.Events.onKeyDown keyPressedDecoder
        , Browser.Events.onKeyUp keyReleasedDecoder
        ]


keyPressedDecoder : Decode.Decoder Msg
keyPressedDecoder =
    Decode.map (toKey >> KeyPressedMsg) (Decode.field "key" Decode.string)


keyReleasedDecoder : Decode.Decoder Msg
keyReleasedDecoder =
    Decode.map (toKey >> KeyReleasedMsg) (Decode.field "key" Decode.string)


toKey : String -> KeyEventMsg
toKey event_key_string =
    case event_key_string of
        "Control" ->
            KeyEventControl

        "Shift" ->
            KeyEventShift

        "Alt" ->
            KeyEventAlt

        "Meta" ->
            KeyEventMeta

        string_ ->
            case String.uncons string_ of
                Just ( char, "" ) ->
                    KeyEventLetter char

                _ ->
                    KeyEventUnknown event_key_string


find_matching_records : Item -> InventoryRecord -> Bool
find_matching_records to_match ( recorded_item, _ ) =
    to_match == recorded_item


can_afford_item : ShopTrends -> Int -> TradeOrder -> Bool
can_afford_item shop_trends held_gold { item, qty } =
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


remove_item_from_inventory_records : InventoryRecords -> Item -> Int -> InventoryRecords
remove_item_from_inventory_records records item qty =
    List.map (reduce_if_matched item qty) records


update_item_type_sentiment item_type_sentiment item_type sentiment_delta =
    Dict.update
        (item_type_to_id item_type)
        (\maybe_sent ->
            case maybe_sent of
                Just existing_sent ->
                    Just (existing_sent + sentiment_delta)

                Nothing ->
                    Just (1.0 + sentiment_delta)
        )
        item_type_sentiment


reduce_if_matched item qty ( i, iq ) =
    if i == item && iq >= qty then
        ( i, iq - qty )
        --TODO spend gp for buying

    else
        ( i, iq )


has_items_to_sell inventory_records item qty =
    List.length
        (List.filter
            (\( i, q ) ->
                q >= qty && find_matching_records item ( i, q )
            )
            inventory_records
        )
        > 0


{-| Gives items from character to other

NOTE: assumes the can\_afford checks etc have been done

-}
trade_items_from_party_to_other : ShopTrends -> Character -> Character -> TradeOrder -> ( ShopTrends, Character, Character )
trade_items_from_party_to_other shop_trends from_character to_character { item, qty } =
    let
        total_cost =
            get_adjusted_item_cost shop_trends item qty

        new_to_items =
            add_item_to_inventory_records to_character.held_items item qty

        new_from_items =
            remove_item_from_inventory_records from_character.held_items item qty

        sentiment_delta =
            if from_character.party == ShopParty then
                0.1

            else if to_character.party == ShopParty then
                -0.1

            else
                0.0

        new_its =
            update_item_type_sentiment
                shop_trends.item_type_sentiment
                item.item_type
                sentiment_delta

        log_entry : ItemTradeLog
        log_entry =
            { item_id = item.id
            , quantity = qty
            , gold_cost = total_cost
            , from_party = from_character.party
            , to_party = to_character.party
            }

        new_item_trade_logs =
            shop_trends.item_trade_logs
                ++ [ log_entry ]
    in
    ( { shop_trends
        | item_type_sentiment = new_its
        , item_trade_logs = new_item_trade_logs
      }
    , { from_character | held_items = new_from_items }
    , { to_character | held_items = new_to_items }
    )


sell_items_from_party_to_other : ShopTrends -> Character -> Character -> TradeOrder -> ( ShopTrends, Character, Character )
sell_items_from_party_to_other shop_trends from_party to_party { item, qty } =
    let
        has_items =
            has_items_to_sell from_party.held_items item qty

        can_afford =
            can_afford_item shop_trends to_party.held_gold { item = item, qty = qty }
    in
    if has_items && can_afford then
        let
            ( new_shop_trends, new_from_party_, new_to_party_ ) =
                trade_items_from_party_to_other shop_trends from_party to_party { item = item, qty = qty }

            total_cost =
                get_adjusted_item_cost shop_trends item qty

            new_from_party =
                { new_from_party_
                    | held_gold = new_from_party_.held_gold + total_cost
                }

            new_to_party =
                { new_to_party_
                    | held_gold = new_to_party_.held_gold - total_cost
                }
        in
        ( new_shop_trends, new_from_party, new_to_party )

    else
        ( shop_trends, from_party, to_party )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        MouseEnterShopItem context item ->
            ( { model | hovered_item_in_character = Just item }, Cmd.none )

        MouseLeaveShopItem context item ->
            ( { model | hovered_item_in_character = Nothing }, Cmd.none )

        BuyItem item qty ->
            let
                ( new_shop_trends, new_shop, new_player ) =
                    sell_items_from_party_to_other
                        model.shop_trends
                        model.shop
                        model.player
                        { item = item, qty = qty }
            in
            ( { model
                | shop_trends = new_shop_trends
                , historical_shop_trends = List.append model.historical_shop_trends [ model.shop_trends ]
                , player = new_player
                , shop = new_shop
              }
            , Cmd.none
            )

        SellItem item qty ->
            let
                ( new_shop_trends, new_player, new_shop ) =
                    sell_items_from_party_to_other
                        model.shop_trends
                        model.player
                        model.shop
                        { item = item, qty = qty }
            in
            ( { model
                | shop_trends = new_shop_trends
                , historical_shop_trends = List.append model.historical_shop_trends [ model.shop_trends ]
                , player = new_player
                , shop = new_shop
              }
            , Cmd.none
            )

        StartTrendsHover ->
            ( { model | shop_trends_hovered = True }, Cmd.none )

        EndTrendsHover ->
            ( { model | shop_trends_hovered = False }, Cmd.none )

        TickSecond time ->
            ( update_ai_chars <| update_player { model | ai_tick_time = time }, Cmd.none )

        OnTrendChartHover hovered ->
            ( { model | hovered_trend_chart = hovered }, Cmd.none )

        ToggleShowDebugInventories ->
            ( { model | show_debug_inventories = not model.show_debug_inventories }, Cmd.none )

        KeyPressedMsg key_event_msg ->
            case key_event_msg of
                KeyEventShift ->
                    ( { model | show_charts_in_hovered_item = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyReleasedMsg key_event_msg ->
            case key_event_msg of
                KeyEventShift ->
                    ( { model | show_charts_in_hovered_item = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


{-| adds 1 gold per second. GPM is a misnomer
-}
add_player_gpm : Character -> Character
add_player_gpm player =
    let
        { held_gold } =
            player

        max_gold =
            50
    in
    if held_gold < max_gold then
        { player | held_gold = held_gold + 1 }

    else
        player


update_player : Model -> Model
update_player model =
    let
        { player } =
            model

        { held_gold } =
            player
    in
    { model | player = player |> add_player_gpm }


get_trend_for_item : ShopTrends -> Item -> Float
get_trend_for_item shop_trends item =
    get_sentiment_for_item shop_trends.item_type_sentiment item


get_sentiment_for_item : ItemSentiments -> Item -> Float
get_sentiment_for_item item_type_sentiment item =
    get_sentiment_for_item_type item_type_sentiment item.item_type


get_sentiment_for_item_type : ItemSentiments -> ItemType -> Float
get_sentiment_for_item_type item_type_sentiment item_type =
    Maybe.withDefault 1.0 <|
        Dict.get
            (item_type_to_id item_type)
            item_type_sentiment


nonzero_qty : InventoryRecord -> Bool
nonzero_qty ( item, qty ) =
    qty > 0


ai_buy_item_from_shop : Time.Posix -> ShopTrends -> Character -> Character -> ( ShopTrends, Character, Character )
ai_buy_item_from_shop ai_tick_time shop_trends character shop =
    --TODO decide on an item type to buy, and buy 1.
    -- Maybe, it would be based on the lowest trending one, or one the
    -- character strongly desired or something
    let
        check_can_afford item =
            can_afford_item
                shop_trends
                character.held_gold
                { item = item, qty = 1 }

        check_nonzero_desire item =
            get_sentiment_for_item character.item_types_desired item
                > 0.0

        wanted_items : InventoryRecords
        wanted_items =
            List.filter
                (\( i, q ) -> nonzero_qty ( i, q ) && check_can_afford i && check_nonzero_desire i)
                shop.held_items

        max_trend =
            1.2

        -- item type trends sorted by least trendy
        least_trendy_items : List ( ItemType, Float )
        least_trendy_items =
            List.filterMap
                (\( it_id, trd ) ->
                    id_to_item_type it_id
                        |> Maybe.andThen
                            (if trd < max_trend then
                                \item_type -> Just ( item_type, trd )

                             else
                                always Nothing
                            )
                )
            <|
                List.sortBy
                    Tuple.second
                <|
                    List.filter
                        (\( it_id, trd ) ->
                            let
                                maybe_item_type =
                                    id_to_item_type it_id

                                desire =
                                    case maybe_item_type of
                                        Just item_type ->
                                            get_sentiment_for_item_type
                                                character.item_types_desired
                                                item_type

                                        Nothing ->
                                            1.0
                            in
                            desire > 0
                        )
                        (Dict.toList shop_trends.item_type_sentiment)

        is_buyable : ItemTypeSentiment -> Maybe Item -> Maybe Item
        is_buyable ( item_type, trend ) buyable_item =
            case buyable_item of
                Just buyable ->
                    Just buyable

                Nothing ->
                    case
                        List.filter
                            (Tuple.first
                                >> .item_type
                                >> (==) item_type
                            )
                            wanted_items
                    of
                        [] ->
                            Nothing

                        ( xi, xq ) :: xs ->
                            Just xi

        maybe_item_to_buy : Maybe Item
        maybe_item_to_buy =
            List.foldl
                is_buyable
                Nothing
                least_trendy_items

        ( new_shop_trends, new_shop, new_character ) =
            case maybe_item_to_buy of
                Nothing ->
                    ( shop_trends
                    , shop
                    , append_to_character_action_log character { time = ai_tick_time, log_type = WantedButCouldntTrade WantedToBuy }
                    )

                Just item ->
                    let
                        ( nst, ns, nc ) =
                            sell_items_from_party_to_other
                                shop_trends
                                shop
                                character
                                { item = item, qty = 1 }

                        -- note that this assumes sell_items_from_party_to_other creates a single trade log
                        maybe_item_trade_log : Maybe ItemTradeLog
                        maybe_item_trade_log =
                            List.drop
                                (List.length nst.item_trade_logs - 1)
                                nst.item_trade_logs
                                |> List.head
                    in
                    ( nst
                    , ns
                    , case maybe_item_trade_log of
                        Just item_trade_log ->
                            append_to_character_action_log
                                nc
                                { log_type = Traded item_trade_log, time = ai_tick_time }

                        Nothing ->
                            nc
                    )
    in
    ( new_shop_trends, new_character, new_shop )


ai_sell_item_to_shop : Time.Posix -> ShopTrends -> Character -> Character -> ( ShopTrends, Character, Character )
ai_sell_item_to_shop ai_tick_time shop_trends character shop =
    let
        sellable_items : InventoryRecords
        sellable_items =
            List.filter (\( i, qty ) -> qty > 0) character.held_items

        -- AI needs to trend to be at least above 80% to sell
        sellable_trend =
            0.8

        -- trend is high enough to sell
        untrendy_enough : InventoryRecord -> Bool
        untrendy_enough ( item, qty ) =
            get_trend_for_item shop_trends item >= sellable_trend

        untrendy_items : InventoryRecords
        untrendy_items =
            List.filter untrendy_enough sellable_items

        ( new_shop_trends, new_character, new_shop ) =
            case List.head untrendy_items of
                Nothing ->
                    ( shop_trends
                    , append_to_character_action_log character { log_type = WantedButCouldntTrade WantedToSell, time = ai_tick_time }
                    , shop
                    )

                Just ( item, qty ) ->
                    let
                        ( nst, nc, ns ) =
                            sell_items_from_party_to_other
                                shop_trends
                                shop
                                character
                                { item = item, qty = 1 }

                        -- note that this assumes sell_items_from_party_to_other creates a single trade log
                        maybe_item_trade_log : Maybe ItemTradeLog
                        maybe_item_trade_log =
                            List.drop
                                (List.length nst.item_trade_logs - 1)
                                nst.item_trade_logs
                                |> List.head
                    in
                    ( nst
                    , case maybe_item_trade_log of
                        Just item_trade_log ->
                            append_to_character_action_log
                                nc
                                { log_type = Traded item_trade_log, time = ai_tick_time }

                        Nothing ->
                            nc
                    , ns
                    )
    in
    ( new_shop_trends, new_character, new_shop )


type AiActionChoice
    = NoActionChoice
    | WantsToSell
    | WantsToBuy


lookup_by_char_id : UUID -> List Character -> Maybe Character
lookup_by_char_id char_id characters =
    List.head <| List.filter (\c -> c.char_id == char_id) characters


type alias AiUpdateData =
    { shop_trends : ShopTrends
    , historical_shop_trends : List ShopTrends
    , characters : List Character
    , ai_tick_seed : Random.Seed
    }


append_to_character_action_log : Character -> ActionLog -> Character
append_to_character_action_log character new_log =
    { character | action_log = character.action_log ++ [ new_log ] }


update_ai_chars : Model -> Model
update_ai_chars model =
    let
        old_characters =
            model.characters

        old_shop_trends =
            model.shop_trends

        old_historical_shop_trends =
            model.historical_shop_trends

        ai_tick_time =
            Random.initialSeed <| Time.posixToMillis model.ai_tick_time

        -- loop through all the characters and..
        -- pass the updated shop trends and characters down
        -- if we use foldl, i am not sure how to iterate through all the characters
        --  without re-using them. Answer: iterate through character ids instead
        update_ai : UUID -> AiUpdateData -> AiUpdateData
        update_ai char_id { shop_trends, historical_shop_trends, characters, ai_tick_seed } =
            let
                --TODO: make sure character isn't shop
                maybe_character =
                    lookup_by_char_id char_id characters

                maybe_shop =
                    lookup_by_char_id model.shop.char_id characters
            in
            case ( maybe_character, maybe_shop ) of
                ( Just character, Just shop ) ->
                    let
                        ( ( maybe_chosen_action, _ ), new_seed ) =
                            Random.step
                                (Random.List.choose
                                    (List.repeat 10 WantsToSell
                                        ++ List.repeat 10 WantsToBuy
                                        ++ List.repeat 5 NoActionChoice
                                    )
                                )
                                ai_tick_seed

                        chosen_action =
                            case maybe_chosen_action of
                                Just action ->
                                    action

                                Nothing ->
                                    NoActionChoice

                        ( new_shop_trends_, new_character, new_shop ) =
                            case chosen_action of
                                WantsToSell ->
                                    ai_sell_item_to_shop model.ai_tick_time shop_trends character shop

                                WantsToBuy ->
                                    ai_buy_item_from_shop model.ai_tick_time shop_trends character shop

                                NoActionChoice ->
                                    ( shop_trends
                                    , append_to_character_action_log character { log_type = DidNothing, time = model.ai_tick_time }
                                    , shop
                                    )

                        new_characters_ =
                            List.map
                                (\c ->
                                    if c.char_id == character.char_id then
                                        new_character

                                    else if c.char_id == shop.char_id then
                                        new_shop

                                    else
                                        c
                                )
                                characters

                        new_historical_shop_trends =
                            List.append
                                historical_shop_trends
                                [ new_shop_trends_ ]
                    in
                    { shop_trends = new_shop_trends_, historical_shop_trends = new_historical_shop_trends, characters = new_characters_, ai_tick_seed = new_seed }

                _ ->
                    { shop_trends = shop_trends, historical_shop_trends = historical_shop_trends, characters = characters, ai_tick_seed = ai_tick_seed }

        new_ai_data : AiUpdateData
        new_ai_data =
            List.foldl
                update_ai
                { shop_trends = old_shop_trends, historical_shop_trends = old_historical_shop_trends, characters = old_characters, ai_tick_seed = ai_tick_time }
            <|
                List.map .char_id <|
                    exclude_player_and_shop model old_characters

        final_shop =
            Maybe.withDefault model.shop <|
                List.head <|
                    List.filter
                        (\c -> c.char_id == model.shop.char_id)
                        new_ai_data.characters
    in
    { model
        | shop_trends = new_ai_data.shop_trends
        , historical_shop_trends = new_ai_data.historical_shop_trends
        , characters = new_ai_data.characters
        , shop = final_shop
    }


render_item_type : ItemType -> Element.Element Msg
render_item_type item_type =
    text <| item_type_to_pretty_string item_type


clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


grey_color =
    rgb 0.35 0.35 0.35


very_light_grey_color =
    rgb 0.75 0.75 0.75


light_grey_color =
    rgb 0.55 0.55 0.55


font_grey : Element.Attribute msg
font_grey =
    Font.color <| grey_color


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
            "BUY"

        else
            "Need GP"


shop_sell_button : Bool -> InventoryRecord -> Element Msg
shop_sell_button has_items_to_sell_ ( item, qty ) =
    let
        button_type =
            if has_items_to_sell_ then
                primary_button

            else
                secondary_button
    in
    button_type
        [ Element.transparent <| not has_items_to_sell_
        , width (fill |> Element.minimum 120)
        ]
        (SellItem item 1)
    <|
        if has_items_to_sell_ then
            "SELL"

        else
            "Need GP"


debug_explain =
    let
        do_explain =
            True
    in
    if do_explain then
        explain Debug.todo

    else
        Element.scale 1.0


get_item_type_trend : ItemSentiments -> ItemType -> Float
get_item_type_trend item_type_sentiments item_type =
    case Dict.get (item_type_to_id item_type) item_type_sentiments of
        Just existing_sent ->
            existing_sent

        Nothing ->
            1.0


is_item_trending : ItemSentiments -> Item -> Bool
is_item_trending item_type_sentiments item =
    case Dict.get (item_type_to_id item.item_type) item_type_sentiments of
        Just existing_sent ->
            existing_sent /= 1.0

        Nothing ->
            False


render_single_item_for_sale :
    ( List ShopTrends, ShopTrends, Bool )
    -> Character
    -> Maybe ( CharacterId, Item )
    -> InventoryRecord
    -> ListContext
    -> (InventoryRecord -> Element Msg)
    -> Element.Element Msg
render_single_item_for_sale ( historical_shop_trends, shop_trends, show_charts_in_hovered_item ) { char_id, held_gold } maybe_hovered_item ( item, qty ) context controls_column =
    let
        is_hovered_item =
            case maybe_hovered_item of
                Just ( hovered_char_id, hovered_item ) ->
                    char_id == hovered_char_id && item == hovered_item

                Nothing ->
                    False

        current_price =
            get_adjusted_item_cost shop_trends item 1

        --shown when hovered over item
        expanded_display =
            if is_hovered_item then
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
                    column [ spacing 5, width fill ]
                        [ row [ width fill ]
                            [ paragraph []
                                [ text item.name
                                , text ": "
                                , text item.description
                                ]
                            , if not show_charts_in_hovered_item then
                                el [ Font.italic, alignRight, font_grey, Font.size 12 ] <| text "Hold Shift for more"

                              else
                                Element.none
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
                        , if show_charts_in_hovered_item then
                            el [ paddingXY 20 20 ] <| small_charts_display historical_shop_trends item.item_type

                          else
                            Element.none
                        ]

            else
                Element.none
    in
    row
        [ font_scaled 1
        , width fill
        , spacingXY 5 0

        -- , Element.spaceEvenly
        , Events.onMouseEnter <| MouseEnterShopItem context ( char_id, item )
        , Events.onMouseLeave <| MouseLeaveShopItem context ( char_id, item )
        , Element.below expanded_display
        ]
        [ column [ Element.clip, width (fillPortion 2 |> Element.maximum 150), Font.size 16 ] [ text <| clipText item.name 25 ]
        , column [ portion 1 ] [ render_gp <| get_adjusted_item_cost shop_trends item 1 ]
        , column [ portion 1 ] [ render_item_type item.item_type ]
        , column [ portion 1 ]
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
                            text "NONE"

                else
                    text <| "x" ++ String.fromInt qty
            ]
        , column [ width <| (fillPortion 3 |> Element.maximum 200) ]
            [ el [] <| text <| clipText item.description 24 ]
        , column [ portion 1 ] [ controls_column ( item, qty ) ]
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


render_single_trade_log_entry : ItemDb -> List Character -> ItemTradeLog -> Element msg
render_single_trade_log_entry item_db all_characters trade_log =
    let
        { from_party, to_party, item_id, quantity, gold_cost } =
            trade_log

        maybe_item =
            lookup_item_id item_db item_id

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

        ( ShopParty, _ ) ->
            paragraph []
                [ text <|
                    "Shop --> "
                        ++ trade_party_to_str all_characters to_party
                        ++ " "
                        ++ item_name
                        ++ " ("
                        ++ qty_str
                        ++ ") "
                        ++ "-- value: "
                , Element.el [ Font.color <| rgb 1 0 0 ] <| rendered_cost
                ]

        ( _, ShopParty ) ->
            paragraph []
                [ text <|
                    "Shop <-- "
                        ++ trade_party_to_str all_characters from_party
                        ++ " "
                        ++ item_name
                        ++ " ("
                        ++ qty_str
                        ++ ") "
                        ++ "-- value: "
                , Element.el [ Font.color <| rgb 0 1 0 ] rendered_cost
                ]

        ( _, _ ) ->
            paragraph []
                [ text <|
                    item_name
                        ++ " was traded from "
                        ++ trade_party_to_str all_characters from_party
                        ++ " to "
                        ++ trade_party_to_str all_characters to_party
                        ++ " ("
                        ++ qty_str
                        ++ ") "
                        ++ "was traded for "
                , rendered_cost
                ]


trends_display : ItemDb -> ShopTrends -> List Character -> Bool -> Element.Element Msg
trends_display item_db shop_trends all_characters is_expanded =
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
                Element.none

        specific_trends : List (Element msg)
        specific_trends =
            List.map render_single_popularity <|
                List.filter (Tuple.second >> (/=) 1) <|
                    Dict.toList shop_trends.item_type_sentiment

        rendered_item_trade_logs : List (Element msg)
        rendered_item_trade_logs =
            [ text <|
                "There have been "
                    ++ (String.fromInt <| List.length shop_trends.item_trade_logs)
                    ++ " trades"
            ]

        rendered_popularity : Element.Element msg
        rendered_popularity =
            row [ spacing 15, paddingXY 0 10 ] <|
                summarized
                    :: specific_trends
                    ++ [ Element.el [ font_scaled 2, centerY ] <| text "|" ]
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
                    [ Element.el [ font_grey, Font.size 12 ] <| text "Latest first" ]
                        ++ (List.map
                                (render_single_trade_log_entry item_db all_characters)
                            <|
                                List.take 50 <|
                                    List.reverse shop_trends.item_trade_logs
                           )

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


divider : List (Element msg)
divider =
    [ Element.el [ width fill, paddingXY 50 5 ] <|
        Element.el
            [ width fill
            , border_bottom 1
            , Border.color very_light_grey_color
            ]
        <|
            Element.none
    ]


lookup_item_id_str : ItemDb -> String -> Maybe Item
lookup_item_id_str item_db item_id_str =
    Dict.get item_id_str item_db


lookup_item_id : ItemDb -> ItemId -> Maybe Item
lookup_item_id item_db item_id =
    -- Dict.get (UUID.toString item_id) item_db
    lookup_item_id_str item_db (UUID.toString item_id)


lookup_item_id_str_default : ItemDb -> ItemIdStr -> Item
lookup_item_id_str_default item_db item_id_str =
    case Dict.get item_id_str item_db of
        Just item ->
            item

        Nothing ->
            unset_item_frame


lookup_item_id_default : ItemDb -> ItemId -> Item
lookup_item_id_default item_db item_id =
    lookup_item_id_str_default item_db (UUID.toString item_id)


action_log_to_str : ItemDb -> ActionLog -> String
action_log_to_str item_db action_log =
    case action_log.log_type of
        Traded item_trade_log ->
            let
                traded_prefix =
                    if is_item_trade_log_to_shop item_trade_log then
                        "Sold: "

                    else
                        "Bought: "
            in
            traded_prefix
                ++ (case lookup_item_id item_db item_trade_log.item_id of
                        Nothing ->
                            "Unknown Item"

                        Just item ->
                            item.name
                   )

        WantedButCouldntTrade action ->
            case action of
                WantedToSell ->
                    "Wanted to sell, but couldn't"

                WantedToBuy ->
                    "Wanted to buy, but couldn't"

        DidNothing ->
            "Did nothing"


render_inventory :
    Model
    -> String
    -> Character
    -> ShopTrends
    -> Maybe ( CharacterId, Item )
    -> ListContext
    -> (InventoryRecord -> Element Msg)
    -> Element Msg
render_inventory model header character shop_trends hovered_item context controls_column =
    let
        { char_id, held_items, held_gold } =
            character

        is_shop_context =
            context == ShopItems

        { historical_shop_trends, item_db, show_charts_in_hovered_item } =
            model

        rendered_items =
            List.map
                (\item ->
                    render_single_item_for_sale
                        ( historical_shop_trends, shop_trends, show_charts_in_hovered_item )
                        character
                        hovered_item
                        item
                        context
                        controls_column
                )
            <|
                List.sortBy sort_func held_items

        rendered_desires : List (Element Msg)
        rendered_desires =
            Dict.toList character.item_types_desired
                |> List.filter (Tuple.second >> (\trd -> trd > 0.0))
                |> List.map
                    (\( it_id, trd ) ->
                        text <|
                            "Desires: "
                                ++ (case id_to_item_type it_id of
                                        Just item_type ->
                                            item_type_to_pretty_string item_type

                                        Nothing ->
                                            "Unknown"
                                   )
                    )

        rendered_dislikes : List (Element Msg)
        rendered_dislikes =
            Dict.toList character.item_types_desired
                |> List.filter (Tuple.second >> (\trd -> trd <= 0.0))
                |> List.map
                    (\( it_id, trd ) ->
                        text <|
                            "Dislikes: "
                                ++ (case id_to_item_type it_id of
                                        Just item_type ->
                                            item_type_to_pretty_string item_type

                                        Nothing ->
                                            "Unknown"
                                   )
                    )

        render_single_action_log : ActionLog -> Element Msg
        render_single_action_log log =
            el [] (text <| action_log_to_str item_db log)

        rendered_action_log_items : List (Element Msg)
        rendered_action_log_items =
            if List.length character.action_log > 0 then
                character.action_log
                    |> List.reverse
                    |> List.take 50
                    |> List.map render_single_action_log

            else
                [ text "No actions taken" ]

        rendered_action_log =
            [ column
                [ width fill
                , height fill
                , scrollbars
                , if not is_shop_context && List.length character.action_log > 0 then
                    height (fill |> Element.maximum 600 |> Element.minimum 50)

                  else
                    height fill
                ]
                (if not is_shop_context then
                    rendered_action_log_items

                 else
                    []
                )
            ]
    in
    Element.column [ width fill, spacingXY 0 5, height fill ] <|
        [ Element.row [ font_scaled 2, width fill ]
            [ Element.el [ border_bottom 2 ] <| text header
            , text "   "
            , if not is_shop_context then
                row [ font_scaled 1, centerX ] <|
                    [ text "Held: ", render_gp held_gold ]

              else
                Element.none
            ]
        ]
            ++ rendered_desires
            ++ rendered_dislikes
            ++ rendered_action_log
            ++ (if not is_shop_context && List.length character.action_log > 0 then
                    divider

                else
                    []
               )
            ++ rendered_items


sort_func =
    Tuple.first >> .name


exclude_player_and_shop : { a | player : Character, shop : Character } -> List Character -> List Character
exclude_player_and_shop { player, shop } characters =
    List.filter
        (\c -> c.char_id /= player.char_id && c.char_id /= shop.char_id)
        characters


float_to_percent : Float -> String
float_to_percent flt =
    flt * 100 |> floor |> String.fromInt |> (\str -> str ++ "%")


small_charts_display :
    List ShopTrends
    -> ItemType
    -> Element Msg
small_charts_display historical_shop_trends item_type =
    let
        chart_width =
            600

        chart_height =
            150

        chart_points =
            200

        raw_dataset_len =
            List.length historical_shop_trends

        raw_dataset : List ( Int, ShopTrends )
        raw_dataset =
            List.indexedMap Tuple.pair <|
                if raw_dataset_len > chart_points then
                    List.drop (raw_dataset_len - chart_points) historical_shop_trends

                else
                    historical_shop_trends

        get_x_from_single_datum : TrendChartDatum -> Float
        get_x_from_single_datum =
            Tuple.first >> toFloat

        get_y_from_single_datum : TrendChartDatum -> Float
        get_y_from_single_datum ( idx, it_val ) =
            Tuple.second it_val

        filter_dataset_by_item_type : ShopTrends -> ( ItemType, Float )
        filter_dataset_by_item_type { item_type_sentiment } =
            ( item_type
            , get_item_type_trend item_type_sentiment item_type
            )

        build_filtered_dataset : List TrendChartDatum
        build_filtered_dataset =
            List.map
                (\( idx, s_t ) ->
                    ( idx, filter_dataset_by_item_type s_t )
                )
                raw_dataset

        build_dataset =
            if
                List.all
                    (\( _, shop_trends ) ->
                        get_item_type_trend shop_trends.item_type_sentiment item_type == 1.0
                    )
                    raw_dataset
            then
                C.none

            else
                C.series (\( idx, _ ) -> toFloat idx)
                    [ C.interpolated
                        -- (\trd -> get_y_from_single_datum item_type trd)
                        (\( idx, it_val ) -> Tuple.second it_val)
                        [ CA.monotone ]
                        []
                        |> C.named (item_type_to_pretty_string item_type)
                    ]
                    build_filtered_dataset

        -- raw_dataset
        datasets =
            [ build_dataset ]

        -- chart_attributes : List (CA.Attribute a)
        chart_attributes =
            [ CA.height chart_height
            , CA.width chart_width
            , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
            , CE.onMouseMove OnTrendChartHover (CE.getNearest CI.dots)
            , CE.onMouseLeave (OnTrendChartHover [])
            , CA.domain
                [ CA.lowest 0.5 CA.orLower
                , CA.highest 1.5 CA.orHigher
                , CA.centerAt 1.0
                ]
            ]

        chart_elements : List (C.Element TrendChartDatum msg)
        chart_elements =
            [ C.xLabels []
            , C.yLabels [ CA.format float_to_percent, CA.withGrid ]
            ]
                ++ datasets
                ++ [ C.legendsAt .min
                        .max
                        [ CA.column
                        , CA.moveRight 15
                        , CA.spacing 5
                        ]
                        [ CA.width 20 ]
                   ]
    in
    Element.el
        [ width <| Element.px chart_width
        , height <| Element.px 100
        , paddingXY 20 0
        , centerX
        ]
    <|
        Element.html <|
            C.chart
                chart_attributes
                chart_elements


charts_display :
    List ShopTrends
    -> List (CI.One TrendChartDatum CI.Dot)
    -> Element Msg
charts_display historical_shop_trends hovered_trend_chart =
    let
        chart_width =
            700

        chart_height =
            150

        chart_points =
            200

        raw_dataset_len =
            List.length historical_shop_trends

        raw_dataset : List ( Int, ShopTrends )
        raw_dataset =
            List.indexedMap Tuple.pair <|
                if raw_dataset_len > chart_points then
                    List.drop (raw_dataset_len - chart_points) historical_shop_trends

                else
                    historical_shop_trends

        get_x_from_single_datum : TrendChartDatum -> Float
        get_x_from_single_datum =
            Tuple.first >> toFloat

        get_y_from_single_datum : TrendChartDatum -> Float
        get_y_from_single_datum ( idx, it_val ) =
            Tuple.second it_val

        -- render_tooltip : p -> CI.One TrendChartDatum CI.Dot -> a
        render_tooltip plane item =
            let
                ( id, it_val ) =
                    CI.getData item

                ( item_type_, val ) =
                    it_val

                -- _ =
                --     Debug.log "item" <| CI.getData item
                item_type =
                    Weapon
            in
            [ C.tooltip item
                []
                []
                [ Html.text <|
                    item_type_to_pretty_string item_type_
                        ++ ": "
                        ++ float_to_percent val
                ]
            ]

        filter_dataset_by_item_type : ItemType -> ShopTrends -> ( ItemType, Float )
        filter_dataset_by_item_type item_type { item_type_sentiment } =
            ( item_type
            , get_item_type_trend item_type_sentiment item_type
            )

        build_filtered_dataset : ItemType -> List TrendChartDatum
        build_filtered_dataset item_type =
            List.map
                (\( idx, s_t ) ->
                    ( idx, filter_dataset_by_item_type item_type s_t )
                )
                raw_dataset

        build_dataset item_type =
            if
                List.all
                    (\( _, shop_trends ) ->
                        get_item_type_trend shop_trends.item_type_sentiment item_type == 1.0
                    )
                    raw_dataset
            then
                C.none

            else
                C.series (\( idx, _ ) -> toFloat idx)
                    [ C.interpolated
                        -- (\trd -> get_y_from_single_datum item_type trd)
                        (\( idx, it_val ) -> Tuple.second it_val)
                        [ CA.monotone ]
                        []
                        |> C.named (item_type_to_pretty_string item_type)
                    ]
                    (build_filtered_dataset item_type)

        -- raw_dataset
        datasets =
            [ build_dataset Weapon
            , build_dataset Armor
            , build_dataset Spellbook
            , build_dataset Furniture
            , build_dataset Food
            ]

        -- chart_attributes : List (CA.Attribute a)
        chart_attributes =
            [ CA.height chart_height
            , CA.width chart_width
            , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
            , CE.onMouseMove OnTrendChartHover (CE.getNearest CI.dots)
            , CE.onMouseLeave (OnTrendChartHover [])
            , CA.domain
                [ CA.lowest 0.5 CA.orLower
                , CA.highest 1.5 CA.orHigher
                , CA.centerAt 1.0
                ]
            ]

        chart_elements : List (C.Element TrendChartDatum msg)
        chart_elements =
            [ C.xLabels []
            , C.yLabels [ CA.format float_to_percent, CA.withGrid ]
            ]
                ++ datasets
                ++ [ C.legendsAt .min
                        .max
                        [ CA.column
                        , CA.moveRight 15
                        , CA.spacing 5
                        ]
                        [ CA.width 20 ]
                   , C.each hovered_trend_chart <| render_tooltip
                   ]
    in
    Element.el
        [ width <| Element.px chart_width
        , height <| Element.px (chart_height + 20)
        , paddingXY 20 0
        ]
    <|
        Element.html <|
            C.chart
                chart_attributes
                chart_elements


view : Model -> Html.Html Msg
view model =
    let
        welcome_header =
            Element.el [ font_scaled 3, padding_bottom 10 ] <| text "Welcome to the Item Shop!"

        debug_inventories : List (Element Msg)
        debug_inventories =
            exclude_player_and_shop model model.characters
                |> List.sortBy (.char_id >> UUID.toString)
                |> List.map
                    (\character ->
                        Element.el [ height fill, paddingXY 0 10, width fill ]
                            (render_inventory
                                model
                                (character.name ++ "'s Inventory")
                                character
                                model.shop_trends
                                model.hovered_item_in_character
                                CharacterItems
                                (always Element.none)
                            )
                    )
    in
    Element.layoutWith { options = [] }
        []
    <|
        Element.column [ width fill, font_scaled 1, height fill ] <|
            [ welcome_header
            , Element.el [ paddingXY 0 10, width fill ] <| charts_display model.historical_shop_trends model.hovered_trend_chart
            , trends_display model.item_db model.shop_trends model.characters model.shop_trends_hovered
            , Element.el [ paddingXY 0 10, width fill ] <|
                render_inventory
                    model
                    "Items For Sale"
                    model.shop
                    model.shop_trends
                    model.hovered_item_in_character
                    ShopItems
                    (\( item, qty ) -> shop_buy_button (get_adjusted_item_cost model.shop_trends item 1) model.player.held_gold ( item, qty ))
            , Element.el [ paddingXY 0 10, width fill ] <|
                render_inventory
                    model
                    "Items In Inventory"
                    model.player
                    model.shop_trends
                    model.hovered_item_in_character
                    InventoryItems
                    (\( item, qty ) -> shop_sell_button (qty >= 1) ( item, 1 ))
            ]
                ++ (if model.show_debug_inventories then
                        [ column [ width fill ]
                            ([ secondary_button [] ToggleShowDebugInventories "Hide Debug"
                             ]
                                ++ debug_inventories
                            )
                        ]

                    else
                        [ secondary_button [] ToggleShowDebugInventories "Show Debug" ]
                   )


scaled : Int -> Int
scaled val =
    modular 14 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale
