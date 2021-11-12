module ItemShop exposing (Model, Msg, init, subscriptions, update, view)

import Array
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


type Msg
    = Noop
    | MouseEnterShopItem ListContext Item
    | MouseLeaveShopItem ListContext Item
    | BuyItem Item Int
    | SellItem Item Int
    | StartTrendsHover
    | EndTrendsHover
    | TickSecond Time.Posix
      -- | OnTrendChartHover (List (CI.One TrendSnapshot CI.Dot))
    | OnTrendChartHover (List (CI.One ( Int, ShopTrends ) CI.Dot))


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
    }


type alias TrendSnapshot =
    { time : Time.Posix, item_type : ItemType, value : Float }


type alias Model =
    { player : Character
    , shop : Character
    , characters : List Character
    , hovered_item_for_sale : Maybe Item
    , hovered_item_in_inventory : Maybe Item
    , hovered_item_in_character : Maybe Item
    , shop_trends : ShopTrends
    , historical_shop_trends : List ShopTrends
    , shop_trends_hovered : Bool

    -- , hovered_trend_chart : List (CI.One TrendSnapshot CI.Dot)
    , hovered_trend_chart : List (CI.One ( Int, ShopTrends ) CI.Dot)
    , ai_tick_time : Time.Posix --used to seed the ai randomness
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
          , { name = "Book of the Dead"
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


initial_items_for_sale : InventoryRecords
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


initial_owned_items : InventoryRecords
initial_owned_items =
    [ ( Maybe.withDefault unset_item_frame <| Dict.get "boots" item_frames, 12 )
    ]


initial_character : Character
initial_character =
    let
        base_character =
            create_character (UUID.forName "character 1" UUID.dnsNamespace) "Billy"
    in
    { base_character
        | held_items =
            [ ( Maybe.withDefault unset_item_frame <| Dict.get "dagger" item_frames, 8 )
            ]
        , held_gold = 100
        , item_types_desired = Dict.fromList [(item_type_to_id Weapon, 0.0)]
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
    }


init : ( Model, Cmd Msg )
init =
    let
        player_base_char =
            create_character (UUID.forName "player character" UUID.dnsNamespace) "Player"

        shop_base_char =
            create_character (UUID.forName "shop character" UUID.dnsNamespace) "Shop"

        player : Character
        player =
            { player_base_char
                | held_items = initial_owned_items
                , held_gold = 0
                , party = PlayerParty
            }

        shop : Character
        shop =
            { shop_base_char
                | held_items = initial_items_for_sale
                , held_gold = 999999999
                , party = ShopParty
            }

        character : Character
        character =
            initial_character
    in
    ( { player = player
      , shop = shop
      , characters = [ player, shop, character ]
      , hovered_item_for_sale = Nothing
      , hovered_item_in_inventory = Nothing
      , hovered_item_in_character = Nothing
      , shop_trends = initial_shop_trends
      , historical_shop_trends = []
      , shop_trends_hovered = False
      , ai_tick_time = Time.millisToPosix -1
      , hovered_trend_chart = []
      }
    , Task.perform TickSecond Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Time.every 1000 TickSecond


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
            -- let
            --     _ =
            --         Debug.log "tick" time
            -- in
            ( update_ai_chars { model | ai_tick_time = time }, Cmd.none )

        OnTrendChartHover hovered ->
            ( { model | hovered_trend_chart = hovered }, Cmd.none )


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


ai_buy_item_from_shop : ShopTrends -> Character -> Character -> ( ShopTrends, Character, Character )
ai_buy_item_from_shop shop_trends character shop =
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

        -- _ =
        --     Debug.log "maybe item to buy" maybe_item_to_buy
        ( new_shop_trends, new_shop, new_character ) =
            case maybe_item_to_buy of
                Nothing ->
                    ( shop_trends, shop, character )

                Just item ->
                    sell_items_from_party_to_other
                        shop_trends
                        shop
                        character
                        { item = item, qty = 1 }
    in
    ( new_shop_trends, new_character, new_shop )


ai_sell_item_to_shop : ShopTrends -> Character -> Character -> ( ShopTrends, Character, Character )
ai_sell_item_to_shop shop_trends character shop =
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
                    ( shop_trends, character, shop )

                Just ( item, qty ) ->
                    sell_items_from_party_to_other
                        shop_trends
                        character
                        shop
                        { item = item, qty = 1 }
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
                                    ai_sell_item_to_shop shop_trends character shop

                                WantsToBuy ->
                                    ai_buy_item_from_shop shop_trends character shop

                                NoActionChoice ->
                                    ( shop_trends, character, shop )

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
                                old_characters

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
            False
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
    ShopTrends
    -> Int
    -> Maybe Item
    -> InventoryRecord
    -> ListContext
    -> (InventoryRecord -> Element Msg)
    -> Element.Element Msg
render_single_item_for_sale shop_trends gold_in_pocket maybe_hovered_item ( item, qty ) context controls_column =
    let
        is_hovered_item =
            case maybe_hovered_item of
                Just hovered_item ->
                    item == hovered_item

                Nothing ->
                    False

        current_price =
            get_adjusted_item_cost shop_trends item 1

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
        [ column [ Element.clip, width (fillPortion 2 |> Element.maximum 150), Font.size 16, debug_explain ] [ text <| clipText item.name 25 ]
        , column [ portion 1, debug_explain ] [ render_gp <| get_adjusted_item_cost shop_trends item 1 ]
        , column [ portion 1, debug_explain ] [ render_item_type item.item_type ]
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
                            text "NONE"

                else
                    text <| "x" ++ String.fromInt qty
            ]
        , column [ width <| (fillPortion 3 |> Element.maximum 200), debug_explain ]
            [ el [] <| text <| clipText item.description 24 ]
        , column [ portion 1, debug_explain ] [ controls_column ( item, qty ) ]
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


render_single_trade_log_entry : List Character -> ItemTradeLog -> Element msg
render_single_trade_log_entry all_characters trade_log =
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


trends_display : ShopTrends -> List Character -> Bool -> Element.Element Msg
trends_display shop_trends all_characters is_expanded =
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
                                (render_single_trade_log_entry all_characters)
                            <|
                                List.take 50 <| List.reverse shop_trends.item_trade_logs
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


render_inventory :
    String
    -> Character
    -> ShopTrends
    -> Maybe Item
    -> ListContext
    -> (InventoryRecord -> Element Msg)
    -> Element Msg
render_inventory header { held_items, held_gold } shop_trends hovered_item context controls_column =
    let
        rendered_items =
            List.map
                (\item ->
                    render_single_item_for_sale
                        shop_trends
                        held_gold
                        hovered_item
                        item
                        context
                        controls_column
                )
            <|
                List.sortBy sort_func held_items
    in
    Element.column [ width fill, spacingXY 0 5 ] <|
        [ Element.row [ font_scaled 2, width fill ]
            [ Element.el [ border_bottom 2 ] <| text header
            , text "   "
            , row [ font_scaled 1, centerX ] <|
                [ text "Held: ", render_gp held_gold ]
            ]
        ]
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


charts_display : Model -> Element Msg
charts_display model =
    let
        chart_width =
            1200

        chart_height =
            150

        raw_dataset =
            List.indexedMap Tuple.pair <| List.take 200 model.historical_shop_trends

        -- get_x_from_single_datum = (.time >> Time.posixToMillis >> toFloat)
        get_x_from_single_datum : ( Int, ShopTrends ) -> Float
        get_x_from_single_datum =
            Tuple.first >> toFloat

        get_y_from_single_datum : ItemType -> ( Int, ShopTrends ) -> Float
        get_y_from_single_datum item_type =
            Tuple.second >> .item_type_sentiment >> (\its -> get_item_type_trend its item_type)

        render_tooltip item =
            [ C.tooltip item
                []
                []
                [ Html.text <|
                    (CI.getData item |> (get_y_from_single_datum Weapon >> float_to_percent))
                ]
            ]

        build_dataset item_type =
            if List.all (\( _, shop_trends ) -> get_item_type_trend shop_trends.item_type_sentiment item_type == 1.0) raw_dataset then
                C.none

            else
                C.series get_x_from_single_datum
                    [ C.interpolated
                        (\trd -> get_y_from_single_datum item_type trd)
                        [ CA.monotone ]
                        []
                        |> C.named (item_type_to_pretty_string item_type)
                    ]
                    raw_dataset

        datasets =
            [ build_dataset Weapon
            , build_dataset Armor
            , build_dataset Spellbook
            , build_dataset Furniture
            , build_dataset Food
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
                ([ C.xLabels []
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
                )


view : Model -> Html.Html Msg
view model =
    let
        welcome_header =
            Element.el [ font_scaled 3, padding_bottom 10 ] <| text "Welcome to the Item Shop!"
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        Element.column [ width fill, font_scaled 1 ] <|
            [ welcome_header
            , Element.el [ paddingXY 0 10, width fill ] <| charts_display model
            , trends_display model.shop_trends model.characters model.shop_trends_hovered
            , Element.el [ paddingXY 0 10, width fill ] <|
                render_inventory
                    "Items For Sale"
                    model.shop
                    model.shop_trends
                    model.hovered_item_for_sale
                    ShopItems
                    (\( item, qty ) -> shop_buy_button (get_adjusted_item_cost model.shop_trends item 1) model.player.held_gold ( item, qty ))
            , Element.el [ paddingXY 0 10, width fill ] <|
                render_inventory
                    "Items In Inventory"
                    model.player
                    model.shop_trends
                    model.hovered_item_in_inventory
                    InventoryItems
                    (\( item, qty ) -> shop_sell_button (qty >= 1) ( item, 1 ))
            ]
                ++ (List.map
                        (\character ->
                            Element.el [ paddingXY 0 10, width fill ]
                                (render_inventory
                                    (character.name ++ "'s Inventory")
                                    character
                                    model.shop_trends
                                    model.hovered_item_in_character
                                    CharacterItems
                                    (always Element.none)
                                )
                        )
                    <|
                        exclude_player_and_shop model model.characters
                   )


scaled : Int -> Int
scaled val =
    modular 14 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale
