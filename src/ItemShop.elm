module ItemShop exposing (Model, Msg, add_to_average, init, sub_from_average, subscriptions, suite, update, view)

import Array
import Browser.Dom
import Browser.Events
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Color
import Color.Convert as Convert
import DOM exposing (offsetWidth, target)
import Dict
import Element
    exposing
        ( Color
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
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
import Element.Keyed
import Element.Lazy as Lazy
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Html
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra
import Random
import Random.List
import Task
import Test exposing (..)
import Time
import Tuple3
import UUID exposing (UUID)


type ItemType
    = Weapon
    | Armor
    | Spellbook
    | Furniture
    | Food


allItemTypes : List ItemType
allItemTypes =
    [ Weapon
    , Armor
    , Spellbook
    , Furniture
    , Food
    ]


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


type SpecialEvent
    = EventVeryDesiredItemType (Maybe ItemType)
    | EventLeastDesiredItemType (Maybe ItemType)


type SpecialAction
    = InviteTrader
    | TriggerEvent SpecialEvent
    | TogglePauseAi
    | UnlockItem
    | IncreaseIncome


type Msg
    = Noop
    | MouseEnterShopItem ListContext ( CharacterId, Item )
    | MouseLeaveShopItem ListContext ( CharacterId, Item )
    | PlayerBuyItemFromShop Item Quantity
    | PlayerSellItemToShop Item Quantity
    | StartTrendsHover
    | EndTrendsHover
    | TickSecond Time.Posix
      -- | OnTrendChartHover (List (CI.One TrendSnapshot CI.Dot))
    | ToggleShowMainChart
    | OnTrendChartHover (List (CI.One TrendChartDatum CI.Dot))
    | ToggleShowDebugInventories
    | KeyPressedMsg KeyEventMsg
    | KeyReleasedMsg KeyEventMsg
    | StartTooltipHover String
    | EndTooltipHover String
    | GotTooltipSize (Result Browser.Dom.Error Browser.Dom.Element)
    | OnSpecialAction SpecialAction Price
    | ToggleHideNonZeroRows CharacterId
    | ChangeTabType TabType
    | CycleFilterDisplayedItemsForward CharacterId (Maybe ItemType)
    | CycleFilterDisplayedItemsBackward CharacterId (Maybe ItemType)
    | ScrollViewport
    | GotViewport Browser.Dom.Viewport
    | GotShowDebugElement (Result Browser.Dom.Error Browser.Dom.Element)


type alias TradeOrder =
    { item : Item
    , qty : Quantity
    }


type Quantity
    = Quantity Int


getQuantity : Quantity -> Int
getQuantity qty =
    case qty of
        Quantity val ->
            val


addQuantityInt : Quantity -> Int -> Quantity
addQuantityInt qty val =
    case qty of
        Quantity orig_val ->
            Quantity (orig_val + val)


addQuantity : Quantity -> Quantity -> Quantity
addQuantity qty other_qty =
    case qty of
        Quantity orig_val ->
            Quantity (orig_val + getQuantity other_qty)


subQuantity : Quantity -> Quantity -> Quantity
subQuantity qty other_qty =
    case qty of
        Quantity orig_val ->
            Quantity (orig_val - getQuantity other_qty)


setQuantity : Int -> Quantity
setQuantity qty =
    Quantity qty


type Price
    = Price Int
    | Free


getPrice : Price -> Int
getPrice qty =
    case qty of
        Price val ->
            val

        Free ->
            0


addPriceInt : Price -> Int -> Price
addPriceInt qty val =
    case qty of
        Price orig_val ->
            Price (orig_val + val)

        Free ->
            Price val


addPrice : Price -> Price -> Price
addPrice qty other_qty =
    case qty of
        Price orig_val ->
            Price (orig_val + getPrice other_qty)

        Free ->
            Price (getPrice other_qty)


subPrice : Price -> Price -> Price
subPrice qty other_qty =
    case qty of
        Price orig_val ->
            if (orig_val - getPrice other_qty) <= 0 then
                Free

            else
                Price (orig_val - getPrice other_qty)

        Free ->
            Free


setPrice : Int -> Price
setPrice qty =
    case qty of
        0 ->
            Free

        _ ->
            Price qty


type alias InventoryRecord =
    { item : Item, quantity : Quantity, avg_price : Price }


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
    | FetchedItem ItemId
    | DidNothing


type alias ActionLog =
    { log_type : ActionLogType
    , time : Time.Posix
    }


type alias ItemTradeLog =
    { item_id : ItemId
    , quantity : Quantity
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
    , hide_zero_qty_inv_rows : Bool
    , displayedItemType : Maybe ItemType
    }


type alias Characters =
    List Character


type alias TrendSnapshot =
    { time : Time.Posix, item_type : ItemType, value : Float }


type alias ItemDbTradeStats =
    { times_you_sold : Int
    , times_you_bought : Int
    , times_others_traded : Int
    }


createItemDbTradeStats : ItemDbTradeStats
createItemDbTradeStats =
    ItemDbTradeStats 0 0 0


updateTimesYouSold : ItemDbTradeStats -> Int -> ItemDbTradeStats
updateTimesYouSold item_db_record_trades new_times_you_sold =
    let
        old_times_you_sold =
            item_db_record_trades.times_you_sold
    in
    { item_db_record_trades
        | times_you_sold = old_times_you_sold + new_times_you_sold
    }


updateTimesYouBought : ItemDbTradeStats -> Int -> ItemDbTradeStats
updateTimesYouBought item_db_record_trades new_times_you_bought =
    let
        old_times_you_bought =
            item_db_record_trades.times_you_bought
    in
    { item_db_record_trades
        | times_you_bought = old_times_you_bought + new_times_you_bought
    }


updateTimesOthersTraded : ItemDbTradeStats -> Int -> ItemDbTradeStats
updateTimesOthersTraded item_db_record_trades new_times_others_traded =
    let
        old_times_others_traded =
            item_db_record_trades.times_others_traded
    in
    { item_db_record_trades
        | times_others_traded = old_times_others_traded + new_times_others_traded
    }


type alias ItemDbRecord =
    { item : Item, is_unlocked : Bool, trade_stats : ItemDbTradeStats }


updateTradeStats : ItemDbRecord -> ItemDbTradeStats -> ItemDbRecord
updateTradeStats item_db_record new_trade_stats =
    { item_db_record | trade_stats = new_trade_stats }


type alias ItemDb =
    Dict.Dict ItemIdStr ItemDbRecord


updateItemDbFromTradeRecord : ItemDb -> (ItemDbTradeStats -> Int -> ItemDbTradeStats) -> TradeRecord -> ItemDb
updateItemDbFromTradeRecord item_db record_updater trade_record =
    case trade_record of
        IncompleteTradeRecord _ ->
            item_db

        CompletedTradeRecord trade_context item_trade_log ->
            item_trade_log
                |> (\itr ->
                        { item = lookup_item_id_default item_db itr.item_id
                        , quantity = itr.quantity
                        , avg_price = setPrice itr.gold_cost
                        }
                   )
                |> List.singleton
                |> List.foldl
                    (increment_item_trade_count record_updater)
                    item_db


type alias TooltipData =
    { offset_x : Float
    , offset_y : Float
    , hovered_tooltip_id : String
    }


type HoveredTooltip
    = NoHoveredTooltip
    | HoveredTooltipWithoutOffset TooltipData
    | HoveredTooltipWithOffset TooltipData


type TabType
    = ShopTabType
    | ItemsUnlockedTabType


type PlayerUpgrade
    = AutomaticGPM Int


type PlayerActionLog
    = TookSpecialActionInviteTrader
    | TookSpecialActionTriggerEvent SpecialEvent
    | TookSpecialActionTogglePauseAi
    | TookSpecialActionUnlockItem ItemId


type alias Model =
    { player_id : CharacterId
    , player_upgrades : List PlayerUpgrade
    , shop_id : CharacterId
    , characters : List Character
    , hovered_item_in_character : Maybe ( CharacterId, Item )
    , shop_trends : ShopTrends
    , historical_shop_trends : List ShopTrends
    , historical_player_actions : List PlayerActionLog
    , shop_trends_hovered : Bool
    , item_db : ItemDb
    , show_main_chart : Bool
    , hovered_trend_chart : List (CI.One TrendChartDatum CI.Dot)
    , ai_tick_time : Time.Posix --used to seed the ai randomness
    , show_debug_inventories : Bool
    , show_charts_in_hovered_item : Bool
    , shiftIsPressed : Bool
    , hovered_tooltip : HoveredTooltip
    , cached_tooltip_offsets : Dict.Dict String TooltipData
    , global_seed : Random.Seed --used to seed anything; will be constantly changed throughout the app
    , ai_updates_paused : Bool
    , tab_type : TabType
    , globalViewport : Maybe Browser.Dom.Viewport
    , showDebugInventoriesElement : Maybe Browser.Dom.Element
    , shouldDisplayShowDebugInventoriesOverlay : Bool
    }


type AiActionChoice
    = NoActionChoice
    | WantsToSell
    | WantsToBuy
    | WantsToFetchItem


{-| the result of what got updated during an AI Update call
-}
type alias AiUpdateData =
    { shop_trends : ShopTrends
    , historical_shop_trends : List ShopTrends
    , characters : List Character
    , ai_tick_seed : Random.Seed
    , item_db : ItemDb
    }


{-| the result of what happens during a ai\_sell/ai\_buy update.
-}
type alias AiUpdateRecord =
    { shop_trends : ShopTrends
    , character : Character
    , shop : Character
    , traded_items : InventoryRecords
    }


type alias TradeContext =
    { shop_trends : ShopTrends
    , from_party : Character
    , to_party : Character
    }


type TradeRecord
    = IncompleteTradeRecord TradeContext
    | CompletedTradeRecord TradeContext ItemTradeLog


type alias TooltipConfig =
    { tooltip_id : String
    , tooltip_text : String
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


create_db_entry : ( Bool, Item ) -> ( String, ItemDbRecord )
create_db_entry ( is_unlocked, item ) =
    ( UUID.toString item.id
    , { item = item
      , is_unlocked = is_unlocked
      , trade_stats = createItemDbTradeStats
      }
    )


initial_item_db : ItemDb
initial_item_db =
    let
        initial_items : List ( Bool, Item )
        initial_items =
            [ ( False
              , { name = "Anklet"
                , item_type = Armor
                , raw_gold_cost = 6
                , description = "An pair of anklets"
                , id = UUID.forName "anklets" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Boots"
                , item_type = Armor
                , raw_gold_cost = 5
                , description = "An old pair of boots"
                , id = UUID.forName "boots" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Leather Armor"
                , item_type = Armor
                , raw_gold_cost = 15
                , description = "Lightly-colored tanned leather"
                , id = UUID.forName "leather armor" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Iron Armor"
                , item_type = Armor
                , raw_gold_cost = 23
                , description = "Medium armor, afford some protection"
                , id = UUID.forName "iron armor" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Iron Helm"
                , item_type = Armor
                , raw_gold_cost = 20
                , description = "A helmet made of iron"
                , id = UUID.forName "iron helm" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Sword"
                , item_type = Weapon
                , raw_gold_cost = 15
                , description = "A rusted sword"
                , id = UUID.forName "rusted sword" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Dagger"
                , item_type = Weapon
                , raw_gold_cost = 10
                , description = "A small weapon that fits in your pocket"
                , id = UUID.forName "small dagger" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Broad-headed axe"
                , item_type = Weapon
                , raw_gold_cost = 19
                , description = "A broad-headed axe, commonly used to fell great trees"
                , id = UUID.forName "broadheaded axe" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Book of the Dead"
                , item_type = Spellbook
                , raw_gold_cost = 20
                , description = "Bound in leather, this book has a skull for a cover"
                , id = UUID.forName "book of the dead" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Book of the Magi"
                , item_type = Spellbook
                , raw_gold_cost = 50
                , description = "Light bounces off strangely, as if it imperceptibly shuddered"
                , id = UUID.forName "book of the magi" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Book of the Mender"
                , item_type = Spellbook
                , raw_gold_cost = 30
                , description = "Gently pulses."
                , id = UUID.forName "book of the mender" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Green Herb"
                , item_type = Food
                , raw_gold_cost = 10
                , description = "Looks like grass."
                , id = UUID.forName "green herb" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Red Herb"
                , item_type = Food
                , raw_gold_cost = 30
                , description = "Looks like angry grass."
                , id = UUID.forName "red herb" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Blue Herb"
                , item_type = Food
                , raw_gold_cost = 50
                , description = "Looks like sad grass."
                , id = UUID.forName "blue herb" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Purple Herb"
                , item_type = Food
                , raw_gold_cost = 40
                , description = "Looks like asphyxiated grass."
                , id = UUID.forName "purple herb" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Meatpie"
                , item_type = Food
                , raw_gold_cost = 25
                , description = "You'd like to eat one of these."
                , id = UUID.forName "meatpie" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Veggiepie"
                , item_type = Food
                , raw_gold_cost = 15
                , description = "You'd not like to eat one of these."
                , id = UUID.forName "veggiepie" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Iron Stool"
                , item_type = Furniture
                , raw_gold_cost = 32
                , description = "Would be great to sit on."
                , id = UUID.forName "iron stool" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Wooden Stool"
                , item_type = Furniture
                , raw_gold_cost = 23
                , description = "Would be okay to sit on."
                , id = UUID.forName "wooden stool" UUID.dnsNamespace
                }
              )
            , ( True
              , { name = "Steel Stool"
                , item_type = Furniture
                , raw_gold_cost = 38
                , description = "Would be better than great to sit on."
                , id = UUID.forName "steel stool" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Strong Stool"
                , item_type = Furniture
                , raw_gold_cost = 55
                , description = "Would be the one sitting on you."
                , id = UUID.forName "strong stool" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Weak Stool"
                , item_type = Furniture
                , raw_gold_cost = 25
                , description = "You'd crush this if you had to sit on it"
                , id = UUID.forName "weak stool" UUID.dnsNamespace
                }
              )
            ]
    in
    initial_items
        |> List.map create_db_entry
        |> Dict.fromList


initial_items_for_sale : ItemDb -> InventoryRecords
initial_items_for_sale item_db =
    let
        seed =
            Random.initialSeed 12345

        item_configs : List ( String, Quantity )
        item_configs =
            [ ( "a41ae9d3-61f0-54f9-800e-56f53ed3ac98", Quantity 3 ) --boots
            , ( "c3c38323-1743-5a47-a8e3-bf6ec28137f9", Quantity 5 )
            , ( "6b7e301d-ab12-5e81-acfc-547e63004ffa", Quantity 4 )
            , ( "48e66792-4c97-598f-8937-3a7042f00591", Quantity 1 )
            ]

        item_records : List ( ItemDbRecord, Quantity )
        item_records =
            List.filterMap
                (\( item_id_str, qty ) ->
                    case lookup_item_id_str item_db item_id_str of
                        Just db_record ->
                            Just ( db_record, qty )

                        Nothing ->
                            Nothing
                )
                item_configs

        -- create the items from the ItemDbRecords
        just_items : InventoryRecords
        just_items =
            List.filterMap
                (\( db_record, qty ) ->
                    if db_record.is_unlocked then
                        Just
                            { item = db_record.item
                            , quantity = qty
                            , avg_price = setPrice db_record.item.raw_gold_cost
                            }

                    else
                        Nothing
                )
                item_records

        debug =
            if List.length item_records == List.length item_configs then
                Ok ""

            else
                --TODO: handling this better, because this isn't elmy at all
                Debug.todo "THERE WAS AN ERROR IN INITIAL ITEM SETUP!!!!" <| Err ""
    in
    just_items


reset_avg_price_to_default : InventoryRecord -> InventoryRecord
reset_avg_price_to_default ({ item } as inventory_record) =
    { inventory_record | avg_price = setPrice item.raw_gold_cost }


initial_owned_items : ItemDb -> InventoryRecords
initial_owned_items item_db =
    [ { item = lookup_item_id_str_default item_db "a41ae9d3-61f0-54f9-800e-56f53ed3ac98", quantity = Quantity 12, avg_price = setPrice 9999 }
    ]
        |> List.map reset_avg_price_to_default


initial_characters : ItemDb -> List Character
initial_characters item_db =
    let
        base_character_1 =
            create_character (generate_uuid "character 1") "Billy"

        base_character_2 =
            create_character (generate_uuid "character 2") "Mitchell"
    in
    [ { base_character_1
        | held_items =
            [ { item =
                    lookup_item_id_str_default item_db
                        "6b7e301d-ab12-5e81-acfc-547e63004ffa"
              , quantity = setQuantity 8
              , avg_price = setPrice 20
              }
            ]
                |> List.map reset_avg_price_to_default
        , held_gold = 100
        , item_types_desired = Dict.fromList [ ( item_type_to_id Weapon, 0.0 ) ]
      }
    , { base_character_2
        | held_items =
            [ { item =
                    lookup_item_id_str_default
                        item_db
                        "a41ae9d3-61f0-54f9-800e-56f53ed3ac98"
              , quantity = setQuantity 12
              , avg_price = setPrice 25
              }
            ]
                |> List.map reset_avg_price_to_default
        , held_gold = 200
        , item_types_desired = Dict.fromList [ ( item_type_to_id Spellbook, 0.0 ) ]
      }
    ]


get_adjusted_item_cost : ShopTrends -> Item -> Quantity -> Int
get_adjusted_item_cost shop_trends { item_type, raw_gold_cost } qty =
    let
        item_sentiment =
            shop_trends.item_type_sentiment
                |> Dict.get (item_type_to_id item_type)
                |> Maybe.withDefault 1.0

        scaled_raw_cost =
            toFloat <| raw_gold_cost * getQuantity qty
    in
    round <| scaled_raw_cost * item_sentiment


get_single_adjusted_item_cost : ShopTrends -> Item -> Int
get_single_adjusted_item_cost shop_trends item =
    get_adjusted_item_cost shop_trends item (Quantity 1)


color_white : Color
color_white =
    rgb 1 1 1


color_black : Color
color_black =
    rgb 0 0 0


hex_to_color : String -> Color
hex_to_color hex_str =
    case Convert.hexToColor hex_str of
        Ok color ->
            let
                -- convert to a Color lib Color record
                rgba =
                    Color.toRgba color
            in
            -- from the Color record, call the ElmUI `rgb` func
            rgb rgba.red rgba.green rgba.blue

        Err err ->
            Debug.todo "NOOO" <| rgb255 255 0 0


{-| lightest green at 1, darkest at 7
-}
color_pastel_green_1 : Color
color_pastel_green_1 =
    hex_to_color "#b4ecb4"


color_pastel_green_2 : Color
color_pastel_green_2 =
    hex_to_color "#a0e7a0"


color_pastel_green_3 : Color
color_pastel_green_3 =
    hex_to_color "#8be28b"


color_pastel_green_4 : Color
color_pastel_green_4 =
    hex_to_color "#77dd77"


color_pastel_green_5 : Color
color_pastel_green_5 =
    hex_to_color "#63d863"


color_pastel_green_6 : Color
color_pastel_green_6 =
    hex_to_color "#4ed34e"


color_pastel_green_7 : Color
color_pastel_green_7 =
    hex_to_color "#3ace3a"


{-| lightest red at 1, darkest at 7
-}
color_pastel_red_1 : Color
color_pastel_red_1 =
    hex_to_color "#ecb4b4"


color_pastel_red_2 : Color
color_pastel_red_2 =
    hex_to_color "#e7a0a0"


color_pastel_red_3 : Color
color_pastel_red_3 =
    hex_to_color "#e28b8b"


color_pastel_red_4 : Color
color_pastel_red_4 =
    hex_to_color "#dd7777"


color_pastel_red_5 : Color
color_pastel_red_5 =
    hex_to_color "#d86363"


color_pastel_red_6 : Color
color_pastel_red_6 =
    hex_to_color "#d34e4e"


color_pastel_red_7 : Color
color_pastel_red_7 =
    hex_to_color "#ce3a3a"


color_secondary : Color
color_secondary =
    hex_to_color "#6c757d"


color_danger : Color
color_danger =
    hex_to_color "#dc3545"


color_primary : Color
color_primary =
    hex_to_color "#007bff"


primary_color_bright : Color
primary_color_bright =
    hex_to_color "#66b0ff"


type alias ButtonConfig =
    { font_color : Color
    , button_color : Color
    , hovered_button_color : Color
    , hovered_font_color : Color
    }


common_button_attrs : ButtonConfig -> List (Element.Attribute Msg)
common_button_attrs { font_color, button_color, hovered_button_color, hovered_font_color } =
    [ -- bs4-like values
      Font.color font_color
    , Font.size 16
    , Font.center
    , padding 6
    , Background.color button_color
    , Border.rounded 5
    , Border.width 5
    , Border.color button_color
    , Element.mouseOver
        [ Background.color <| hovered_button_color
        , Border.color <| primary_color_bright
        , Font.color <| rgb 0 0 0
        ]
    ]


primary_button_custom : List (Element.Attribute Msg) -> Msg -> Element Msg -> Element Msg
primary_button_custom custom_attrs on_press label =
    Input.button
        (common_button_attrs
            { font_color = color_white
            , button_color = color_primary
            , hovered_button_color = primary_color_bright
            , hovered_font_color = color_black
            }
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


primary_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
primary_button custom_attrs on_press label =
    primary_button_custom custom_attrs on_press (text label)


secondary_button_custom : List (Element.Attribute Msg) -> Msg -> Element Msg -> Element Msg
secondary_button_custom custom_attrs on_press label =
    Input.button
        (common_button_attrs
            { font_color = color_white
            , button_color = color_secondary
            , hovered_button_color = color_secondary --_bright
            , hovered_font_color = color_black
            }
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


secondary_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
secondary_button custom_attrs on_press label =
    secondary_button_custom custom_attrs on_press (text label)


outline_button_custom : List (Element.Attribute Msg) -> Msg -> Element Msg -> Element Msg
outline_button_custom custom_attrs on_press label =
    Input.button
        ([ -- bs4-like values
           Font.color color_secondary
         , Font.size 16
         , Font.center
         , padding 6
         , Background.color color_white
         , Border.rounded 5
         , Border.width 2
         , Border.color color_secondary
         , Element.mouseOver
            [ Background.color <| color_secondary
            , Font.color <| color_white
            ]
         ]
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


outline_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
outline_button custom_attrs on_press label =
    outline_button_custom custom_attrs on_press (text label)



-- this doesn't help me so far


scrollbarYEl : List (Element.Attribute msg) -> Element msg -> Element msg
scrollbarYEl custom_attrs body =
    el [ height fill, width fill ] <|
        el
            ([ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
             , Element.htmlAttribute <| Html.Attributes.style "top" "0"
             , Element.htmlAttribute <| Html.Attributes.style "right" "0"
             , Element.htmlAttribute <| Html.Attributes.style "bottom" "0"
             , Element.htmlAttribute <| Html.Attributes.style "left" "0"
             , Element.scrollbarY
             ]
                ++ custom_attrs
            )
            body


danger_button_custom : List (Element.Attribute Msg) -> Msg -> Element Msg -> Element Msg
danger_button_custom custom_attrs on_press label =
    Input.button
        ([ -- bs4-like values
           Font.color color_white
         , Font.size 16
         , Font.center
         , padding 6
         , Background.color color_danger
         , Border.rounded 5
         , Border.width 5
         , Border.color color_danger
         ]
            ++ custom_attrs
        )
        { onPress = Just on_press, label = label }


danger_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
danger_button custom_attrs on_press label_str =
    danger_button_custom custom_attrs on_press <| text label_str


no_text_decoration : Element.Attribute msg
no_text_decoration =
    Element.htmlAttribute <| Html.Attributes.style "text-decoration" "inherit"


inherit_color : Element.Attribute msg
inherit_color =
    Element.htmlAttribute <| Html.Attributes.style "color" "initial"


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
    { -- inventory
      held_items = []
    , held_gold = 0

    -- name and party
    , char_id = char_id
    , name = name
    , party = CharacterParty char_id

    -- trading data
    , trend_tolerance = empty_trend_tolerance
    , item_types_desired = empty_item_sentiments
    , action_log = []

    -- misc
    , hide_zero_qty_inv_rows = False
    , displayedItemType = Nothing
    }


init : String -> ( Model, Cmd Msg )
init hash =
    let
        player_base_char =
            create_character (UUID.forName "player character" UUID.dnsNamespace) "Player"

        shop_base_char =
            create_character (UUID.forName "shop character" UUID.dnsNamespace) "Shop"

        item_db : ItemDb
        item_db =
            initial_item_db

        player : Character
        player =
            { player_base_char
                | held_items = initial_owned_items item_db
                , held_gold = 25
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

        initial_tab_type : TabType
        initial_tab_type =
            case hash of
                "shop" ->
                    ShopTabType

                "items" ->
                    ItemsUnlockedTabType

                _ ->
                    ShopTabType
    in
    ( { player_id = player.char_id
      , player_upgrades = [ AutomaticGPM 1 ]
      , shop_id = shop.char_id
      , characters = characters
      , hovered_item_in_character = Nothing
      , shop_trends = initial_shop_trends
      , item_db = item_db
      , historical_shop_trends = []
      , historical_player_actions = []
      , shop_trends_hovered = False
      , ai_tick_time = Time.millisToPosix -1
      , show_main_chart = True
      , hovered_trend_chart = []
      , show_debug_inventories = True
      , show_charts_in_hovered_item = False
      , shiftIsPressed = False
      , hovered_tooltip = NoHoveredTooltip
      , cached_tooltip_offsets = Dict.empty
      , global_seed = Random.initialSeed 4
      , ai_updates_paused = False
      , tab_type = initial_tab_type
      , globalViewport = Nothing
      , showDebugInventoriesElement = Nothing
      , shouldDisplayShowDebugInventoriesOverlay = False
      }
    , Task.perform TickSecond Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Sub.batch
        [ if not model.ai_updates_paused then
            Time.every 1000 TickSecond

          else
            Sub.none
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
find_matching_records to_match { item } =
    to_match == item


can_afford_item : ShopTrends -> Int -> TradeOrder -> Bool
can_afford_item shop_trends held_gold { item, qty } =
    held_gold >= get_adjusted_item_cost shop_trends item qty


add_to_average : Int -> Int -> Int -> Int -> Int
add_to_average old_avg old_count new_value new_count =
    (old_count * old_avg + new_value) // (old_count + new_count)


sub_from_average : Int -> Int -> Int -> Int -> Int
sub_from_average old_avg old_count new_value new_count =
    (old_count * old_avg - new_value) // (old_count - new_count)


add_item_to_inventory_records : InventoryRecords -> Item -> Quantity -> Int -> InventoryRecords
add_item_to_inventory_records records item qty total_cost =
    case List.filter (find_matching_records item) records of
        [] ->
            -- records ++ [ ( item, qty, setPrice (total_cost // getQuantity qty) ) ]
            records
                ++ [ { item = item
                     , quantity = qty
                     , avg_price =
                        setPrice
                            (add_to_average 0 0 total_cost (getQuantity qty))
                     }
                   ]

        --FIXME this rounds down I think, so it'll cost lost money
        matching_records ->
            let
                updated_records : InventoryRecords
                updated_records =
                    List.map
                        (\({ quantity, avg_price } as ir) ->
                            { item = ir.item
                            , quantity = addQuantity quantity qty
                            , avg_price =
                                setPrice <|
                                    add_to_average
                                        (getPrice avg_price)
                                        (getQuantity quantity)
                                        total_cost
                                        (getQuantity <| addQuantity quantity qty)
                            }
                        )
                        matching_records

                remaining_records =
                    List.filter (not << find_matching_records item) records
            in
            remaining_records ++ updated_records


remove_item_from_inventory_records : InventoryRecords -> Item -> Quantity -> Int -> InventoryRecords
remove_item_from_inventory_records records item qty total_cost =
    List.map (reduce_if_matched item qty total_cost) records


update_item_type_sentiment : ItemSentiments -> ItemType -> Float -> ItemSentiments
update_item_type_sentiment item_type_sentiment item_type sentiment_delta =
    Dict.update
        (item_type_to_id item_type)
        (Maybe.withDefault 1.0 >> (+) sentiment_delta >> Just)
        item_type_sentiment


set_item_type_sentiment : ItemSentiments -> ItemType -> Float -> ItemSentiments
set_item_type_sentiment item_type_sentiment item_type new_val =
    Dict.update
        (item_type_to_id item_type)
        (always <| Just new_val)
        item_type_sentiment


reduce_if_matched : Item -> Quantity -> Int -> InventoryRecord -> InventoryRecord
reduce_if_matched item qty total_cost ({ avg_price } as inventory_record) =
    if inventory_record.item == item && getQuantity inventory_record.quantity >= getQuantity qty then
        let
            newQty =
                subQuantity inventory_record.quantity qty
        in
        { item = inventory_record.item
        , quantity = newQty
        , avg_price =
            if getQuantity newQty == 0 then
                setPrice 0

            else
                avg_price
        }

    else
        inventory_record


has_items_to_sell : InventoryRecords -> Item -> Quantity -> Bool
has_items_to_sell inventory_records item qty =
    List.length
        (List.filter
            (\ir ->
                getQuantity ir.quantity
                    >= getQuantity qty
                    && find_matching_records item ir
            )
            inventory_records
        )
        > 0


takeLast : Int -> List a -> List a
takeLast count list =
    list |> List.reverse |> List.take count |> List.reverse


{-| Gives items from character to other

NOTE: assumes the can\_afford checks etc have been done

-}
trade_items_from_party_to_other : ShopTrends -> Character -> Character -> TradeOrder -> TradeRecord
trade_items_from_party_to_other shop_trends from_character to_character { item, qty } =
    let
        total_cost =
            get_adjusted_item_cost shop_trends item qty

        new_to_items =
            add_item_to_inventory_records
                to_character.held_items
                item
                qty
                total_cost

        new_from_items =
            remove_item_from_inventory_records
                from_character.held_items
                item
                qty
                total_cost

        sentiment_delta =
            if from_character.party == ShopParty then
                0.05

            else if to_character.party == ShopParty then
                -0.05

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
    CompletedTradeRecord
        { shop_trends =
            { shop_trends
                | item_type_sentiment = new_its
                , item_trade_logs = takeLast 100 new_item_trade_logs
            }
        , from_party = { from_character | held_items = new_from_items }
        , to_party = { to_character | held_items = new_to_items }
        }
        log_entry


calc_transaction_fee : Int -> Int
calc_transaction_fee total_cost =
    1


getTradeContext : TradeRecord -> TradeContext
getTradeContext trade_record =
    case trade_record of
        IncompleteTradeRecord context ->
            context

        CompletedTradeRecord context _ ->
            context


isElementOnScreen : Browser.Dom.Viewport -> Browser.Dom.Element -> Bool
isElementOnScreen gvp { element } =
    let
        { y, height } =
            gvp.viewport

        -- _ =
        --     Debug.log "gvp: y height" ( y, height )
        --
        -- _ =
        --     Debug.log "elm: y height" ( element.element.y, element.element.height )
    in
    y > (element.y + element.height)


sell_items_from_party_to_other : TradeContext -> TradeOrder -> TradeRecord
sell_items_from_party_to_other orig_trade_context { item, qty } =
    let
        { shop_trends, from_party, to_party } =
            orig_trade_context

        has_items =
            has_items_to_sell from_party.held_items item qty

        can_afford =
            can_afford_item shop_trends to_party.held_gold { item = item, qty = qty }
    in
    if has_items && can_afford then
        let
            trade_record =
                trade_items_from_party_to_other
                    shop_trends
                    from_party
                    to_party
                    { item = item, qty = qty }

            trade_context =
                getTradeContext trade_record

            new_from_party_ =
                trade_context.from_party

            new_to_party_ =
                trade_context.to_party

            total_cost : Int
            total_cost =
                get_adjusted_item_cost shop_trends item qty

            transaction_fee : Int
            transaction_fee =
                calc_transaction_fee total_cost

            new_from_party : Character
            new_from_party =
                { new_from_party_
                    | held_gold =
                        new_from_party_.held_gold
                            + (total_cost - transaction_fee)
                }

            new_to_party : Character
            new_to_party =
                { new_to_party_
                    | held_gold = new_to_party_.held_gold - total_cost
                }

            new_trade_context =
                trade_context
                    |> (\tc -> { tc | from_party = new_from_party })
                    |> (\tc -> { tc | to_party = new_to_party })
        in
        case trade_record of
            CompletedTradeRecord tc tl ->
                CompletedTradeRecord new_trade_context tl

            IncompleteTradeRecord tc ->
                IncompleteTradeRecord tc

    else
        IncompleteTradeRecord orig_trade_context


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        MouseEnterShopItem context item ->
            ( { model | hovered_item_in_character = Just item }, Cmd.none )

        MouseLeaveShopItem context item ->
            ( { model | hovered_item_in_character = Nothing }, Cmd.none )

        PlayerBuyItemFromShop item qty ->
            case getShop model of
                Just shop ->
                    case getPlayer model of
                        Just player ->
                            let
                                -- ( new_shop_trends, new_shop, new_player ) =
                                trade_record =
                                    sell_items_from_party_to_other
                                        { shop_trends = model.shop_trends
                                        , from_party = shop
                                        , to_party = player
                                        }
                                        { item = item, qty = qty }

                                new_trade_context =
                                    getTradeContext trade_record

                                new_item_db =
                                    updateItemDbFromTradeRecord model.item_db updateTimesYouBought trade_record
                            in
                            ( { model
                                | shop_trends = new_trade_context.shop_trends
                                , historical_shop_trends = List.append model.historical_shop_trends [ model.shop_trends ]
                                , item_db = new_item_db
                              }
                                --it doesn't matter who was what party, they're still getting updated
                                |> withCharacter new_trade_context.to_party
                                |> withCharacter new_trade_context.from_party
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PlayerSellItemToShop item qty ->
            case getShop model of
                Just shop ->
                    case getPlayer model of
                        Just player ->
                            let
                                trade_order =
                                    { item = item, qty = qty }

                                orig_trade_context =
                                    { shop_trends = model.shop_trends
                                    , from_party = player
                                    , to_party = shop
                                    }

                                trade_record =
                                    sell_items_from_party_to_other
                                        orig_trade_context
                                        trade_order

                                new_trade_context =
                                    getTradeContext trade_record

                                new_item_db =
                                    updateItemDbFromTradeRecord model.item_db updateTimesYouSold trade_record
                            in
                            ( { model
                                | shop_trends = new_trade_context.shop_trends
                                , historical_shop_trends = List.append model.historical_shop_trends [ model.shop_trends ]
                                , item_db = new_item_db
                              }
                                |> withCharacter new_trade_context.from_party
                                |> withCharacter new_trade_context.to_party
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        StartTrendsHover ->
            ( { model | shop_trends_hovered = True }, Cmd.none )

        EndTrendsHover ->
            ( { model | shop_trends_hovered = False }, Cmd.none )

        TickSecond time ->
            if not model.ai_updates_paused then
                ( { model | ai_tick_time = time }
                    |> update_player
                    |> update_ai_chars
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ToggleShowMainChart ->
            ( { model | show_main_chart = not model.show_main_chart }, Cmd.none )

        OnTrendChartHover hovered ->
            ( { model | hovered_trend_chart = hovered }, Cmd.none )

        ToggleShowDebugInventories ->
            ( { model | show_debug_inventories = not model.show_debug_inventories }, Cmd.none )

        KeyPressedMsg key_event_msg ->
            case key_event_msg of
                KeyEventShift ->
                    ( { model | show_charts_in_hovered_item = True, shiftIsPressed = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyReleasedMsg key_event_msg ->
            case key_event_msg of
                KeyEventShift ->
                    ( { model | show_charts_in_hovered_item = False, shiftIsPressed = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartTooltipHover tooltip_id ->
            ( { model
                | hovered_tooltip =
                    Dict.get tooltip_id model.cached_tooltip_offsets
                        |> Maybe.withDefault { offset_x = 0, offset_y = 0, hovered_tooltip_id = tooltip_id }
                        |> HoveredTooltipWithoutOffset
              }
            , Task.attempt GotTooltipSize (Browser.Dom.getElement ("tooltip__" ++ tooltip_id))
            )

        EndTooltipHover tooltip_id ->
            ( { model | hovered_tooltip = NoHoveredTooltip }, Cmd.none )

        GotTooltipSize tooltip_size_result ->
            case tooltip_size_result of
                Ok sizes ->
                    let
                        viewport_width =
                            sizes.viewport.width

                        viewport_height =
                            sizes.viewport.height

                        { x, y, width, height } =
                            sizes.element

                        offset_x : Float
                        offset_x =
                            toFloat <|
                                if x < 0 then
                                    floor <| abs x + 10

                                else if x + width > viewport_width then
                                    floor <| (viewport_width - (x + width)) - 10

                                else
                                    floor <| 0

                        offset_y : Float
                        offset_y =
                            toFloat <|
                                if y < 0 then
                                    floor <| abs y + 10

                                else if y + height > viewport_height then
                                    floor <| (viewport_height - (y + height)) - 10

                                else
                                    floor <| 0
                    in
                    case model.hovered_tooltip of
                        NoHoveredTooltip ->
                            ( model, Cmd.none )

                        HoveredTooltipWithoutOffset old_tooltip_data ->
                            let
                                new_tooltip_data =
                                    -- have to add the old offsets back in, because the new tooltip_size_result includes the cached size, so it needs to be accounted for
                                    { offset_x = offset_x + old_tooltip_data.offset_x
                                    , offset_y = offset_y + old_tooltip_data.offset_y
                                    , hovered_tooltip_id = old_tooltip_data.hovered_tooltip_id
                                    }
                            in
                            ( { model
                                | cached_tooltip_offsets = Dict.insert old_tooltip_data.hovered_tooltip_id new_tooltip_data model.cached_tooltip_offsets
                                , hovered_tooltip = HoveredTooltipWithOffset new_tooltip_data
                              }
                            , Cmd.none
                            )

                        HoveredTooltipWithOffset old_tooltip_data ->
                            let
                                new_tooltip_data =
                                    { old_tooltip_data
                                        | offset_x = offset_x
                                        , offset_y = offset_y
                                    }
                            in
                            ( { model | hovered_tooltip = HoveredTooltipWithOffset new_tooltip_data }
                            , Cmd.none
                            )

                Err error ->
                    ( model, Cmd.none )

        OnSpecialAction special_action price ->
            update_special_action special_action price model

        ToggleHideNonZeroRows char_id ->
            let
                new_characters =
                    List.map
                        (\char ->
                            if char.char_id /= char_id then
                                char

                            else
                                { char
                                    | hide_zero_qty_inv_rows = not char.hide_zero_qty_inv_rows
                                }
                        )
                        model.characters
            in
            ( { model | characters = new_characters }, Cmd.none )

        ChangeTabType tab_type ->
            ( { model | tab_type = tab_type }, Cmd.none )

        CycleFilterDisplayedItemsForward character_id mb_item_type ->
            let
                maybeCharacter =
                    getCharacter model.characters character_id
            in
            case maybeCharacter of
                Just character ->
                    let
                        getIdx : Int -> Maybe ItemType
                        getIdx idx =
                            List.Extra.getAt idx allItemTypes

                        newItemType =
                            case mb_item_type of
                                Nothing ->
                                    getIdx 0

                                Just item_type ->
                                    let
                                        curIdx : Maybe Int
                                        curIdx =
                                            List.Extra.elemIndex item_type allItemTypes
                                    in
                                    Maybe.map ((+) 1) curIdx
                                        |> Maybe.andThen getIdx
                    in
                    ( withCharacter { character | displayedItemType = newItemType } model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CycleFilterDisplayedItemsBackward character_id mb_item_type ->
            let
                maybeCharacter =
                    getCharacter model.characters character_id
            in
            case maybeCharacter of
                Just character ->
                    let
                        getIdx : Int -> Maybe ItemType
                        getIdx idx =
                            List.Extra.getAt idx allItemTypes

                        newItemType =
                            case mb_item_type of
                                Nothing ->
                                    getIdx 0

                                Just item_type ->
                                    let
                                        curIdx : Maybe Int
                                        curIdx =
                                            List.Extra.elemIndex item_type allItemTypes
                                    in
                                    Maybe.map (\i -> i - 1) curIdx
                                        |> Debug.log "curIdx"
                                        |> Maybe.andThen getIdx
                    in
                    ( withCharacter { character | displayedItemType = newItemType } model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ScrollViewport ->
            let
                _ =
                    Debug.log "Scroll" 123
            in
            ( model, Task.perform GotViewport Browser.Dom.getViewport )

        GotViewport viewport ->
            let
                _ =
                    Debug.log "got viewport" viewport
            in
            ( { model | globalViewport = Just viewport }, Task.attempt GotShowDebugElement (Browser.Dom.getElement "show_debug_inventories") )

        GotShowDebugElement attemptedElement ->
            let
                _ =
                    Debug.log "got debug viewport" attemptedElement

                ( modelElement, shouldDisplayShowDebugInventoriesOverlay ) =
                    case attemptedElement of
                        Ok element ->
                            let
                                _ =
                                    Debug.log "element" element

                                _ =
                                    Debug.log "model.globalViewport" model.globalViewport

                                _ =
                                    Debug.log "bottom of element is offscreen" <| shouldDisplay

                                shouldDisplay =
                                    Maybe.map
                                        (\gvp -> isElementOnScreen gvp element)
                                        model.globalViewport
                                        |> Maybe.withDefault False
                            in
                            ( Just element, shouldDisplay )

                        Err _ ->
                            ( Nothing, False )
            in
            ( { model
                | showDebugInventoriesElement =
                    modelElement
                , shouldDisplayShowDebugInventoriesOverlay =
                    shouldDisplayShowDebugInventoriesOverlay
              }
            , Cmd.none
            )



--- END OF UPDATE


generate_uuid : String -> UUID.UUID
generate_uuid str =
    UUID.forName str UUID.dnsNamespace


pick_random_unlocked_item_from_db : ItemDb -> Random.Seed -> ( Maybe Item, Random.Seed )
pick_random_unlocked_item_from_db item_db seed =
    let
        ( ( maybe_item, _ ), new_seed ) =
            Dict.values item_db
                |> List.filter .is_unlocked
                |> Random.List.choose
                |> (\gen -> Random.step gen seed)
    in
    ( Maybe.map .item maybe_item, new_seed )


pick_random_locked_item_from_db : ItemDb -> Random.Seed -> ( Maybe Item, Random.Seed )
pick_random_locked_item_from_db item_db seed =
    let
        ( ( maybe_item, _ ), new_seed ) =
            Dict.values item_db
                |> List.filter (not << .is_unlocked)
                |> Random.List.choose
                |> (\gen -> Random.step gen seed)
    in
    ( Maybe.map .item maybe_item, new_seed )


tuple_swap : ( a, b ) -> ( b, a )
tuple_swap pair =
    ( Tuple.second pair, Tuple.first pair )


pick_item :
    ItemDb
    ->
        (a
         -> ( Random.Seed, List (Maybe InventoryRecord) )
         -> ( Random.Seed, List (Maybe InventoryRecord) )
        )
pick_item item_db _ ( prev_seed, folded_items ) =
    let
        ( maybe_item, seed_ ) =
            pick_random_unlocked_item_from_db item_db prev_seed

        result =
            Maybe.map
                (\item ->
                    { item = item
                    , quantity = Quantity 1
                    , avg_price = setPrice item.raw_gold_cost
                    }
                )
                maybe_item
    in
    ( seed_, result :: folded_items )


handle_invite_trader : Model -> Model
handle_invite_trader model =
    let
        { characters, item_db, global_seed } =
            model

        name =
            "Character " ++ (String.fromInt <| List.length model.characters + 1)

        invited_character =
            create_character (generate_uuid name) name

        ( num_items, _ ) =
            Random.step (Random.int 1 5) global_seed

        ( new_global_seed, held_maybe_item_frames ) =
            List.foldl
                (pick_item item_db)
                ( global_seed, [] )
                (List.repeat num_items ())

        incr_if_matches : Item -> InventoryRecord -> InventoryRecord
        incr_if_matches item ir =
            if ir.item.id == item.id then
                { item = ir.item, quantity = addQuantityInt ir.quantity 1, avg_price = ir.avg_price }

            else
                { item = ir.item, quantity = ir.quantity, avg_price = ir.avg_price }

        held_items : InventoryRecords
        held_items =
            List.filterMap identity held_maybe_item_frames
                |> List.foldl
                    (\{ item, quantity, avg_price } prev_items ->
                        if
                            List.any
                                (.item >> .id >> (==) item.id)
                                prev_items
                        then
                            List.map (incr_if_matches item) prev_items

                        else
                            { item = item, quantity = quantity, avg_price = avg_price } :: prev_items
                    )
                    []
    in
    { model
        | characters =
            List.append
                characters
                [ { invited_character
                    | held_gold = 50
                    , held_items = held_items
                  }
                ]
        , global_seed = new_global_seed
    }
        |> append_player_action_log TookSpecialActionInviteTrader


append_player_action_log : PlayerActionLog -> Model -> Model
append_player_action_log new_player_action_log model =
    { model
        | historical_player_actions =
            List.append
                model.historical_player_actions
                [ new_player_action_log ]
    }


handle_special_event : Model -> SpecialEvent -> Model
handle_special_event model spec_event =
    let
        { shop_trends, historical_shop_trends } =
            model

        { item_type_sentiment } =
            shop_trends

        choose_item_type : Random.Seed -> ( ( Maybe ItemType, List ItemType ), Random.Seed )
        choose_item_type seed =
            Random.step
                (Random.List.choose
                    (List.repeat 10 Weapon
                        ++ List.repeat 10 Armor
                        ++ List.repeat 10 Spellbook
                    )
                )
                seed
    in
    case spec_event of
        EventVeryDesiredItemType item_type ->
            let
                ( ( maybe_chosen_item_type, _ ), new_seed ) =
                    choose_item_type model.global_seed
            in
            update_shop_trends model
                (\st ->
                    { st
                        | item_type_sentiment =
                            set_item_type_sentiment
                                st.item_type_sentiment
                                (Maybe.withDefault Weapon maybe_chosen_item_type)
                                1.5
                    }
                )
                |> setGlobalSeed new_seed
                |> append_player_action_log (TookSpecialActionTriggerEvent <| EventVeryDesiredItemType maybe_chosen_item_type)

        EventLeastDesiredItemType _ ->
            let
                ( ( maybe_chosen_item_type, _ ), new_seed ) =
                    choose_item_type model.global_seed
            in
            update_shop_trends model
                (\st ->
                    { st
                        | item_type_sentiment =
                            set_item_type_sentiment
                                st.item_type_sentiment
                                (Maybe.withDefault Weapon maybe_chosen_item_type)
                                0.5
                    }
                )
                |> setGlobalSeed new_seed
                |> append_player_action_log (TookSpecialActionTriggerEvent <| EventLeastDesiredItemType maybe_chosen_item_type)


setGlobalSeed : Random.Seed -> Model -> Model
setGlobalSeed new_seed model =
    { model | global_seed = new_seed }


{-| Takes Model and sets its shop\_trends and appends the new one to the historical trends
-}
update_shop_trends : Model -> (ShopTrends -> ShopTrends) -> Model
update_shop_trends model update_st_func =
    let
        new_shop_trends =
            update_st_func model.shop_trends

        new_historical_shop_trends =
            List.append
                model.historical_shop_trends
                [ new_shop_trends ]
    in
    { model
        | shop_trends = new_shop_trends
        , historical_shop_trends = new_historical_shop_trends
    }


special_action_increase_income : Model -> Model
special_action_increase_income model =
    case getPlayer model of
        Nothing ->
            model

        Just player ->
            let
                automaticGpmLevel =
                    model.player_upgrades
                        |> List.filterMap
                            (\pu ->
                                case pu of
                                    AutomaticGPM level ->
                                        Just level
                            )
                        |> List.head
                        |> Maybe.withDefault 1

                upgradeCost =
                    getPrice <| scale_increase_income_cost automaticGpmLevel
            in
            if player.held_gold >= upgradeCost then
                model
                    |> withCharacter { player | held_gold = player.held_gold - upgradeCost }
                    |> (\m ->
                            { m
                                | player_upgrades =
                                    List.map
                                        (\pu ->
                                            case pu of
                                                AutomaticGPM level ->
                                                    AutomaticGPM <| level + 1
                                        )
                                        m.player_upgrades
                            }
                       )

            else
                model


special_action_unlock_item : Model -> Model
special_action_unlock_item model =
    let
        { item_db, ai_tick_time } =
            model

        ( mb_item_to_unlock, new_seed ) =
            pick_random_locked_item_from_db item_db <| seed_from_time ai_tick_time
    in
    { model
        | item_db =
            case mb_item_to_unlock of
                Just item_to_unlock ->
                    update_item_db item_db
                        item_to_unlock.id
                        (Maybe.map (\idbr -> { idbr | is_unlocked = True }))

                Nothing ->
                    item_db
    }
        |> (case mb_item_to_unlock of
                Just item_to_unlock ->
                    append_player_action_log (TookSpecialActionUnlockItem item_to_unlock.id)

                Nothing ->
                    identity
           )


hasEnoughGold : Character -> Price -> Bool
hasEnoughGold character price =
    character.held_gold >= getPrice price


update_special_action : SpecialAction -> Price -> Model -> ( Model, Cmd Msg )
update_special_action special_action price model =
    getPlayer model
        |> Maybe.andThen
            (\player ->
                if hasEnoughGold player price then
                    Just player

                else
                    Nothing
            )
        |> Maybe.map
            (\player ->
                model
                    |> withCharacter { player | held_gold = player.held_gold - getPrice price }
                    |> (\new_model ->
                            case special_action of
                                InviteTrader ->
                                    ( new_model
                                        |> handle_invite_trader
                                        |> handle_invite_trader
                                        |> handle_invite_trader
                                        |> handle_invite_trader
                                        |> handle_invite_trader
                                        |> handle_invite_trader
                                        |> handle_invite_trader
                                        |> handle_invite_trader
                                    , Cmd.none
                                    )

                                TriggerEvent event ->
                                    ( handle_special_event new_model event, Cmd.none )

                                TogglePauseAi ->
                                    ( { new_model | ai_updates_paused = not new_model.ai_updates_paused } |> append_player_action_log TookSpecialActionTogglePauseAi, Cmd.none )

                                UnlockItem ->
                                    ( special_action_unlock_item model, Cmd.none )

                                IncreaseIncome ->
                                    ( special_action_increase_income model, Cmd.none )
                       )
            )
        |> Maybe.withDefault ( model, Cmd.none )


{-| adds 1 gold per second. GPM is a misnomer
-}
add_player_gpm : Character -> Int -> Character
add_player_gpm player to_add =
    let
        { held_gold } =
            player

        max_gold =
            50
    in
    if held_gold < max_gold then
        { player | held_gold = min max_gold <| held_gold + to_add }

    else
        player


apply_upgrade : PlayerUpgrade -> ( Character, Model ) -> ( Character, Model )
apply_upgrade upgrade ( player, model ) =
    let
        new_player =
            case upgrade of
                AutomaticGPM to_add ->
                    add_player_gpm player to_add
    in
    ( new_player, withCharacter new_player model )


apply_upgrades : Character -> Model -> Model
apply_upgrades player model =
    let
        ( new_player, new_model ) =
            List.foldl apply_upgrade ( player, model ) model.player_upgrades
    in
    new_model


update_player : Model -> Model
update_player model =
    case getPlayer model of
        Just player ->
            apply_upgrades player model

        Nothing ->
            Debug.log "error: cant find player" model


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
nonzero_qty inventory_record =
    getQuantity inventory_record.quantity > 0


seed_from_time : Time.Posix -> Random.Seed
seed_from_time time =
    Random.initialSeed <| Time.posixToMillis time


group_shuffle_items : Random.Seed -> List a -> List a
group_shuffle_items seed items =
    -- Shuffle around the items in groups of 3 to randomize their choices more
    -- TODO: figure out if this works better than shuffling groups of three starting from the first
    items
        |> List.Extra.greedyGroupsOf 6
        |> List.foldl
            (\grp ( old_seed, acc ) ->
                let
                    ( shuffled, new_seed ) =
                        Random.step (Random.List.shuffle grp) old_seed
                in
                ( new_seed, acc ++ shuffled )
            )
            ( seed, [] )
        |> Tuple.second


check_nonzero_desire : Character -> Item -> Bool
check_nonzero_desire character item =
    let
        desire =
            get_sentiment_for_item
                character.item_types_desired
                item
    in
    desire > 0.0


{-| can the character afford the current price for the item
-}
check_can_afford_one : Character -> ShopTrends -> Item -> Bool
check_can_afford_one character shop_trends item =
    can_afford_item
        shop_trends
        character.held_gold
        { item = item, qty = Quantity 1 }


signedFromInt : Int -> String
signedFromInt int =
    if int > 0 then
        "+" ++ String.fromInt int

    else
        String.fromInt int


convertColor : Color.Color -> Element.Color
convertColor color =
    Element.fromRgb <| Color.toRgba <| color


colorFromInt : Int -> Color -> Color -> Color -> Color
colorFromInt int positiveColor neutralColor negativeColor =
    if int > 0 then
        positiveColor

    else if int == 0 then
        neutralColor

    else
        negativeColor


{-| items the character can afford and desires at least a little
-}
get_wanted_items : Character -> Character -> ShopTrends -> InventoryRecords
get_wanted_items character shop shop_trends =
    List.filter
        (\inventory_record ->
            nonzero_qty inventory_record
                && check_can_afford_one character shop_trends inventory_record.item
                && check_nonzero_desire character inventory_record.item
        )
        shop.held_items


ai_buy_item_from_shop : Time.Posix -> ItemDb -> ShopTrends -> Character -> Character -> AiUpdateRecord
ai_buy_item_from_shop ai_tick_time item_db shop_trends character shop =
    --TODO decide on an item type to buy, and buy 1.
    -- Maybe, it would be based on the lowest trending one, or one the
    -- character strongly desired or something
    let
        wanted_items : InventoryRecords
        wanted_items =
            get_wanted_items character shop shop_trends

        max_trend =
            1.4

        sentiment_above_zero : ItemTypeId -> Bool
        sentiment_above_zero item_type_id =
            let
                maybe_item_type =
                    id_to_item_type item_type_id

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

        is_item_trendy_enough : ItemTypeIdSentiment -> Maybe ItemTypeSentiment
        is_item_trendy_enough ( it_id, trd ) =
            id_to_item_type it_id
                |> Maybe.andThen
                    (if trd < max_trend then
                        \item_type -> Just ( item_type, trd )

                     else
                        always Nothing
                    )

        -- item type trends sorted by least trendy
        least_trendy_items : List ( ItemType, Float )
        least_trendy_items =
            Dict.toList shop_trends.item_type_sentiment
                |> List.filter
                    (Tuple.first >> sentiment_above_zero)
                |> List.sortBy
                    Tuple.second
                |> List.filterMap
                    is_item_trendy_enough

        is_item_wanted : ItemTypeSentiment -> Maybe Item -> Maybe Item
        is_item_wanted ( item_type, trend ) buyable_item =
            case buyable_item of
                Just buyable ->
                    Just buyable

                Nothing ->
                    case
                        List.filter
                            (.item
                                >> .item_type
                                >> (==) item_type
                            )
                            wanted_items
                    of
                        [] ->
                            Nothing

                        { item } :: rest_sentiments ->
                            Just item

        qty_to_buy : Quantity
        qty_to_buy =
            Quantity 1

        -- for all the least trendy items in the shop,
        --  find one that is in wanted_items
        maybe_item_to_buy : Maybe Item
        maybe_item_to_buy =
            List.foldl
                is_item_wanted
                Nothing
                (group_shuffle_items (seed_from_time ai_tick_time) least_trendy_items)

        trade_record : TradeRecord
        trade_record =
            case maybe_item_to_buy of
                Nothing ->
                    IncompleteTradeRecord
                        { shop_trends = shop_trends
                        , from_party = shop
                        , to_party =
                            append_to_character_action_log
                                character
                                { time = ai_tick_time
                                , log_type = WantedButCouldntTrade WantedToBuy
                                }
                        }

                Just item ->
                    sell_items_from_party_to_other
                        { shop_trends = shop_trends
                        , from_party = shop
                        , to_party = character
                        }
                        { item = item, qty = qty_to_buy }
    in
    case trade_record of
        IncompleteTradeRecord trade_context_ ->
            { shop_trends = trade_context_.shop_trends
            , character = trade_context_.to_party
            , shop = trade_context_.from_party
            , traded_items = []
            }

        CompletedTradeRecord trade_context_ log ->
            { shop_trends = trade_context_.shop_trends
            , character =
                append_to_character_action_log
                    trade_context_.to_party
                    { log_type = Traded log, time = ai_tick_time }
            , shop = trade_context_.from_party
            , traded_items =
                case lookup_item_id item_db log.item_id of
                    Just item_trade_log ->
                        [ { item = item_trade_log.item, quantity = log.quantity, avg_price = Price log.gold_cost } ]

                    Nothing ->
                        -- []
                        Debug.todo ""
            }



-- tradeRecordToAiUpdate


ai_sell_item_to_shop : Time.Posix -> ItemDb -> ShopTrends -> Character -> Character -> AiUpdateRecord
ai_sell_item_to_shop ai_tick_time item_db shop_trends character shop =
    let
        sellable_items : InventoryRecords
        sellable_items =
            List.filter (\{ quantity } -> getQuantity quantity > 0) character.held_items

        -- AI needs to trend to be at least above 80% to sell
        sellable_trend =
            0.8

        -- trend is high enough to sell
        untrendy_enough : InventoryRecord -> Bool
        untrendy_enough { item } =
            get_trend_for_item shop_trends item >= sellable_trend

        profitable_enough : InventoryRecord -> Bool
        profitable_enough { item } =
            get_single_adjusted_item_cost shop_trends item > 0

        untrendy_items : InventoryRecords
        untrendy_items =
            List.filter untrendy_enough sellable_items

        wanted_to_sell_but_couldnt =
            { log_type = WantedButCouldntTrade WantedToSell
            , time = ai_tick_time
            }

        shuffled_items_to_sell =
            (case untrendy_items of
                [] ->
                    List.filter profitable_enough sellable_items

                _ ->
                    untrendy_items
            )
                |> group_shuffle_items (seed_from_time ai_tick_time)

        qty_to_sell : Quantity
        qty_to_sell =
            Quantity 1

        trade_record : TradeRecord
        trade_record =
            case List.head shuffled_items_to_sell of
                Nothing ->
                    IncompleteTradeRecord
                        { shop_trends = shop_trends
                        , from_party =
                            append_to_character_action_log
                                character
                                wanted_to_sell_but_couldnt
                        , to_party = shop
                        }

                Just { item } ->
                    sell_items_from_party_to_other
                        { shop_trends = shop_trends
                        , from_party = character
                        , to_party = shop
                        }
                        { item = item, qty = qty_to_sell }
    in
    case trade_record of
        IncompleteTradeRecord trade_context_ ->
            { shop_trends = trade_context_.shop_trends
            , character = trade_context_.from_party
            , shop = trade_context_.to_party
            , traded_items = []
            }

        CompletedTradeRecord trade_context_ log ->
            { shop_trends = trade_context_.shop_trends
            , character =
                append_to_character_action_log
                    trade_context_.from_party
                    { log_type = Traded log, time = ai_tick_time }
            , shop = trade_context_.to_party
            , traded_items =
                case lookup_item_id item_db log.item_id of
                    Just item_trade_log ->
                        [ { item = item_trade_log.item, quantity = log.quantity, avg_price = Price log.gold_cost } ]

                    Nothing ->
                        Debug.todo "" []
            }


append_to_character_action_log : Character -> ActionLog -> Character
append_to_character_action_log character new_log =
    let
        new_action_log =
            (character.action_log ++ [ new_log ])
                |> List.reverse
                |> List.take 100
                |> List.reverse
    in
    { character | action_log = new_action_log }



-- loop through all the characters and..
-- pass the updated shop trends and characters down
-- if we use foldl, i am not sure how to iterate through all the characters
--  without re-using them. Answer: iterate through character ids instead


increment_item_trade_count : (ItemDbTradeStats -> Int -> ItemDbTradeStats) -> InventoryRecord -> ItemDb -> ItemDb
increment_item_trade_count record_updater inventory_record item_db =
    let
        added_qty =
            inventory_record |> .quantity |> getQuantity

        item : Item
        item =
            inventory_record.item
    in
    --set or update the traded quantity in the matching ItemDbRecord
    update_item_db
        item_db
        item.id
        (\mb_item_db_record ->
            case mb_item_db_record of
                Just ({ trade_stats } as item_db_record) ->
                    Just
                        (record_updater
                            trade_stats
                            added_qty
                            |> updateTradeStats item_db_record
                        )

                Nothing ->
                    Just { item = item, is_unlocked = False, trade_stats = { createItemDbTradeStats | times_others_traded = added_qty } }
        )


ai_fetch_item : Time.Posix -> ItemDb -> ShopTrends -> Character -> Character -> AiUpdateRecord
ai_fetch_item ai_tick_time item_db shop_trends ({ held_items } as character) shop =
    let
        --note we don't use the newSeed here. we should use global_seed, so that all AIs dont do the same thing
        ( mbNewItem, newSeed ) =
            pick_random_unlocked_item_from_db item_db (seed_from_time ai_tick_time)
    in
    { shop_trends = shop_trends
    , character =
        case mbNewItem of
            Just newItem ->
                character
                    |> (\c ->
                            { c
                                | held_items =
                                    add_item_to_inventory_records
                                        held_items
                                        newItem
                                        (setQuantity 1)
                                        newItem.raw_gold_cost
                            }
                       )
                    |> (\c ->
                            append_to_character_action_log c
                                { log_type = FetchedItem newItem.id, time = ai_tick_time }
                       )

            Nothing ->
                character
    , shop = shop
    , traded_items = []
    }


pickAiActionChoice : Random.Seed -> ( AiActionChoice, Random.Seed )
pickAiActionChoice ai_tick_seed =
    (List.repeat 10 WantsToSell
        ++ List.repeat 10 WantsToBuy
        ++ List.repeat 2 WantsToFetchItem
        ++ List.repeat 5 NoActionChoice
    )
        |> Random.List.choose
        |> (\choices -> Random.step choices ai_tick_seed)
        |> Tuple.mapFirst
            (Tuple.first >> Maybe.withDefault NoActionChoice)


update_ai : Time.Posix -> CharacterId -> CharacterId -> AiUpdateData -> AiUpdateData
update_ai ai_tick_time shop_char_id char_id ({ shop_trends, historical_shop_trends, characters, ai_tick_seed, item_db } as original_ai_update_data) =
    let
        --TODO: make sure character isn't shop
        maybe_character =
            getCharacter characters char_id

        maybe_shop =
            getCharacter characters shop_char_id
    in
    case ( maybe_character, maybe_shop ) of
        ( Just character, Just shop ) ->
            let
                -- chosen_action, new_seed : (AiActionChoice, Random.Seed)
                ( chosen_action, new_seed ) =
                    pickAiActionChoice ai_tick_seed

                ai_update_record : AiUpdateRecord
                ai_update_record =
                    case chosen_action of
                        WantsToSell ->
                            ai_sell_item_to_shop
                                ai_tick_time
                                item_db
                                shop_trends
                                character
                                shop

                        WantsToBuy ->
                            ai_buy_item_from_shop
                                ai_tick_time
                                item_db
                                shop_trends
                                character
                                shop

                        WantsToFetchItem ->
                            ai_fetch_item
                                ai_tick_time
                                item_db
                                shop_trends
                                character
                                shop

                        NoActionChoice ->
                            { shop_trends = shop_trends
                            , character =
                                append_to_character_action_log character
                                    { log_type = DidNothing, time = ai_tick_time }
                            , shop = shop
                            , traded_items = []
                            }

                new_characters : Characters
                new_characters =
                    List.map
                        (\c ->
                            if c.char_id == character.char_id then
                                ai_update_record.character

                            else if c.char_id == shop.char_id then
                                ai_update_record.shop

                            else
                                c
                        )
                        characters

                new_historical_shop_trends =
                    List.append
                        historical_shop_trends
                        [ ai_update_record.shop_trends ]

                new_item_db =
                    --update item db with counts traded_items
                    case ai_update_record.traded_items of
                        [] ->
                            item_db

                        inventory_records ->
                            List.foldl
                                (increment_item_trade_count updateTimesOthersTraded)
                                item_db
                                inventory_records
            in
            { shop_trends = ai_update_record.shop_trends
            , historical_shop_trends = new_historical_shop_trends
            , characters = new_characters
            , ai_tick_seed = new_seed
            , item_db = new_item_db
            }

        _ ->
            Debug.log "ERRORRRRR no matching characters" <|
                original_ai_update_data


update_ai_chars : Model -> Model
update_ai_chars model =
    let
        old_characters =
            model.characters

        old_shop_trends =
            model.shop_trends

        old_historical_shop_trends =
            model.historical_shop_trends

        ai_tick_seed =
            Random.initialSeed <| Time.posixToMillis model.ai_tick_time

        { ai_tick_time, item_db, shop_id } =
            model

        first_ai_update_data =
            { shop_trends = old_shop_trends
            , historical_shop_trends = old_historical_shop_trends
            , characters = old_characters
            , ai_tick_seed = ai_tick_seed
            , item_db = item_db
            }

        new_ai_data : AiUpdateData
        new_ai_data =
            old_characters
                |> exclude_player_and_shop model
                |> List.map .char_id
                |> List.foldl
                    (update_ai ai_tick_time shop_id)
                    first_ai_update_data
    in
    { model
        | shop_trends = new_ai_data.shop_trends
        , historical_shop_trends = new_ai_data.historical_shop_trends
        , characters = new_ai_data.characters
        , item_db = new_ai_data.item_db
    }


get_trend_color : Float -> Color
get_trend_color trend =
    if trend > 1.65 then
        color_pastel_red_7

    else if trend > 1.55 then
        color_pastel_red_6

    else if trend > 1.45 then
        color_pastel_red_5

    else if trend > 1.35 then
        color_pastel_red_4

    else if trend > 1.25 then
        color_pastel_red_3

    else if trend > 1.15 then
        color_pastel_red_2

    else if trend > 1.0 then
        color_pastel_red_1

    else if trend < 0.45 then
        color_pastel_green_7

    else if trend < 0.55 then
        color_pastel_green_6

    else if trend < 0.65 then
        color_pastel_green_5

    else if trend < 0.75 then
        color_pastel_green_4

    else if trend < 0.85 then
        color_pastel_green_3

    else if trend < 0.95 then
        color_pastel_green_2

    else if trend < 1.0 then
        color_pastel_green_1

    else
        rgb 0 0 0


render_item_type : ShopTrends -> ItemType -> Element.Element Msg
render_item_type shop_trends item_type =
    let
        trend =
            get_item_type_trend shop_trends.item_type_sentiment item_type

        pretty_trend =
            String.fromInt (round (trend * 100)) ++ "%"

        trend_color =
            get_trend_color trend

        trend_shadow =
            if trend /= 1.0 then
                [ Font.shadow { offset = ( 1, 1 ), blur = 0.25, color = rgb 0 0 0 } ]

            else
                []
    in
    Element.paragraph [ Font.alignLeft, width fill ] <|
        [ text <|
            item_type_to_pretty_string item_type
        , text " - "
        , el ([ Font.color trend_color ] ++ trend_shadow) <| text pretty_trend
        ]


clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


color_grey : Color
color_grey =
    rgb 0.35 0.35 0.35


color_very_light_grey : Color
color_very_light_grey =
    rgb 0.75 0.75 0.75


color_very_very_light_grey : Color
color_very_very_light_grey =
    rgb 0.85 0.85 0.85


color_ultra_light_grey : Color
color_ultra_light_grey =
    rgb 0.95 0.95 0.95


color_light_grey : Color
color_light_grey =
    rgb 0.55 0.55 0.55


font_grey : Element.Attribute msg
font_grey =
    Font.color <| color_grey


render_gp : Int -> Element msg
render_gp count =
    render_gp_sized count 12


render_gp_string : Int -> String
render_gp_string count =
    String.fromInt count ++ "gp"


render_gp_sized : Int -> Int -> Element msg
render_gp_sized count font_size =
    paragraph []
        [ text <| String.fromInt count
        , Element.el [ Font.size font_size, font_grey ] (text "gp")
        ]


shop_buy_button : Int -> Int -> InventoryRecord -> Element Msg
shop_buy_button gold_cost gold_in_pocket { item, quantity, avg_price } =
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
        [ getQuantity quantity < 1 |> Element.transparent
        , width (fill |> Element.minimum 120)
        ]
        (PlayerBuyItemFromShop item (Quantity 1))
    <|
        if can_afford then
            "BUY"

        else
            "Need GP"


shop_sell_button : Bool -> InventoryRecord -> Element Msg
shop_sell_button has_items_to_sell_ { item } =
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
        (PlayerSellItemToShop item (Quantity 1))
    <|
        if has_items_to_sell_ then
            "SELL"

        else
            "Need GP"


explain =
    Element.explain Debug.todo


debug_explain : Element.Attribute msg
debug_explain =
    let
        do_explain =
            True
    in
    if do_explain then
        explain

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


html_title : String -> Element.Attribute msg
html_title string =
    -- string |> Html.Attributes.title |> Element.htmlAttribute
    Element.behindContent Element.none


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
            String.fromInt (getQuantity quantity) |> (++) "x"

        rendered_cost : Element msg
        rendered_cost =
            render_gp gold_cost

        item_name =
            case maybe_item of
                Just db_record ->
                    db_record.item.name

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


monospace attrs el =
    Element.el (Font.family [ Font.monospace ] :: attrs) el


trends_display : Bool -> ItemDb -> ShopTrends -> List Character -> Bool -> Element.Element Msg
trends_display shiftIsPressed item_db shop_trends all_characters is_expanded =
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
            paragraph []
                [ text <| pretty_type
                , text ": "
                , monospace [] <|
                    text <|
                        ((popularity * 100)
                            |> round
                            |> String.fromInt
                            |> String.padLeft 3 '\u{2003}'
                            |> (\pct -> pct ++ "%")
                        )
                ]

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
                -- List.filter (Tuple.second >> (/=) 1) <|
                Dict.toList shop_trends.item_type_sentiment

        rendered_item_trade_logs : List (Element msg)
        rendered_item_trade_logs =
            [ text <|
                "There have been "
                    ++ (String.fromInt <| List.length shop_trends.item_trade_logs)
                    ++ " trades, and "
                    ++ (String.fromInt <| List.length all_characters)
                    ++ " traders."
            ]

        rendered_popularity : Element.Element Msg
        rendered_popularity =
            Element.column [ spacing 5, paddingXY 0 10, width fill ] <|
                summarized
                    :: [ row [ Element.spaceEvenly, width fill ] specific_trends ]
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
                    [ row [ font_grey, Font.size 12, width fill, Element.spaceEvenly ]
                        [ text "Latest first"
                        , if shiftIsPressed then
                            Element.none

                          else
                            text "Hold shift for more"
                        ]
                    ]
                        ++ (List.map
                                (render_single_trade_log_entry item_db all_characters)
                            <|
                                List.take
                                    (if shiftIsPressed then
                                        50

                                     else
                                        5
                                    )
                                <|
                                    List.reverse shop_trends.item_trade_logs
                           )

        has_trades =
            List.length shop_trends.item_trade_logs > 0
    in
    column
        ([ width fill ]
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
            , Border.color color_very_light_grey
            ]
        <|
            Element.none
    ]


update_item_db : ItemDb -> ItemId -> (Maybe ItemDbRecord -> Maybe ItemDbRecord) -> ItemDb
update_item_db item_db item_id updater =
    Dict.update (UUID.toString item_id) updater item_db


lookup_item_id_str : ItemDb -> String -> Maybe ItemDbRecord
lookup_item_id_str item_db item_id_str =
    Dict.get item_id_str item_db


lookup_item_id : ItemDb -> ItemId -> Maybe ItemDbRecord
lookup_item_id item_db item_id =
    -- Dict.get (UUID.toString item_id) item_db
    lookup_item_id_str item_db (UUID.toString item_id)


lookup_item_id_str_default : ItemDb -> ItemIdStr -> Item
lookup_item_id_str_default item_db item_id_str =
    case Dict.get item_id_str item_db of
        Just db_record ->
            db_record.item

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

                        Just db_record ->
                            db_record.item.name
                   )

        WantedButCouldntTrade action ->
            case action of
                WantedToSell ->
                    "Wanted to sell, but couldn't"

                WantedToBuy ->
                    "Wanted to buy, but couldn't"

        FetchedItem itemId ->
            "Fetched an item"

        DidNothing ->
            "Did nothing"


render_inventory_grid :
    Model
    -> String
    -> Character
    -> ShopTrends
    -> Maybe ( CharacterId, Item )
    -> ListContext
    -> (InventoryRecord -> Element Msg)
    -> Element Msg
render_inventory_grid model header character shop_trends hovered_item context controls_column =
    let
        { char_id, held_items, held_gold, hide_zero_qty_inv_rows } =
            character

        is_shop_context =
            context == ShopItems

        { historical_shop_trends, item_db, show_charts_in_hovered_item } =
            model

        items : InventoryRecords
        items =
            (if hide_zero_qty_inv_rows then
                List.filter
                    (\{ quantity } -> getQuantity quantity > 0)
                    held_items

             else
                held_items
            )
                |> List.sortBy sort_func
                |> (\irs ->
                        case character.displayedItemType of
                            Nothing ->
                                irs

                            Just item_type ->
                                List.filter
                                    (\ir -> ir.item.item_type == item_type)
                                    irs
                   )

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

        rendered_inventory_controls : List (Element Msg)
        rendered_inventory_controls =
            [ row [ spacingXY 10 0 ]
                [ primary_button []
                    (ToggleHideNonZeroRows character.char_id)
                    (if hide_zero_qty_inv_rows then
                        "Show Nonzero"

                     else
                        "Hide Nonzero"
                    )
                , secondary_button
                    [ Html.Events.preventDefaultOn "contextmenu"
                        (Decode.succeed <| ( CycleFilterDisplayedItemsBackward character.char_id character.displayedItemType, True ))
                        |> Element.htmlAttribute
                    ]
                    (CycleFilterDisplayedItemsForward character.char_id character.displayedItemType)
                  <|
                    "Filter: "
                        ++ (case character.displayedItemType of
                                Nothing ->
                                    "All"

                                Just itemType ->
                                    item_type_to_pretty_string itemType
                           )
                ]
            ]

        is_hovered_item item =
            case hovered_item of
                Just ( hovered_char_id, hovered_item_ ) ->
                    char_id == hovered_char_id && item == hovered_item_

                Nothing ->
                    False

        current_price item =
            get_single_adjusted_item_cost shop_trends item

        --shown when hovered over item
        expanded_display item =
            if is_hovered_item item then
                Element.Keyed.el
                    [ width fill
                    , Background.color <| rgb 1 1 1
                    , Border.color <| rgb 0.35 0.35 0.35
                    , Border.rounded 3
                    , Border.width 2
                    , padding 10
                    , Element.moveDown 20
                    ]
                <|
                    ( UUID.toString item.id
                      -- , text "POOPY"
                    , column [ spacing 5, width fill ]
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
                            , render_gp (current_price item)
                            ]
                                ++ (if
                                        is_item_trending
                                            shop_trends.item_type_sentiment
                                            item
                                            && item.raw_gold_cost
                                            /= current_price item
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
                    )

            else
                Element.none

        mouse_hover_attrs : Item -> List (Element.Attribute Msg)
        mouse_hover_attrs item =
            [ Events.onMouseEnter <| MouseEnterShopItem context ( char_id, item )
            , Events.onMouseLeave <| MouseLeaveShopItem context ( char_id, item )

            -- , Element.below (expanded_display item)
            ]

        small_header str =
            el [ Font.size 10 ] <| text str

        table_columns : List (Element.Column InventoryRecord Msg)
        table_columns =
            [ { header = small_header "Name:"
              , width = fillPortion 2
              , view =
                    \{ item, quantity, avg_price } ->
                        Element.el
                            (mouse_hover_attrs item
                                ++ [ width (fillPortion 2 |> Element.maximum 150)
                                   , Font.size 16
                                   , html_title "Item Name"
                                   , centerY
                                   ]
                            )
                            (text (clipText item.name 25))
              }
            , { header = small_header "Price"
              , width = fillPortion 1
              , view =
                    \{ item, quantity, avg_price } ->
                        Element.el
                            [ portion 1, centerY, html_title "Current cost" ]
                        <|
                            let
                                adjustedPrice =
                                    get_single_adjusted_item_cost shop_trends item

                                priceDiff =
                                    adjustedPrice - getPrice avg_price
                            in
                            paragraph [] <|
                                [ render_gp <|
                                    get_single_adjusted_item_cost shop_trends item
                                ]
                                    ++ [ if context /= ShopItems && priceDiff /= 0 && getQuantity quantity /= 0 then
                                            let
                                                diffColor =
                                                    colorFromInt priceDiff (convertColor Color.green) color_black color_danger
                                            in
                                            el [ Font.size 12, Font.color diffColor ] <| text <| " (" ++ signedFromInt priceDiff ++ ")"

                                         else
                                            Element.none
                                       ]
              }
            , { header = small_header "Avg Px"
              , width = fillPortion 1
              , view =
                    \{ quantity, avg_price } ->
                        Element.el
                            [ portion 1, centerY, html_title "Average Cost" ]
                        <|
                            case context of
                                ShopItems ->
                                    text ""

                                _ ->
                                    if getQuantity quantity /= 0 then
                                        render_gp <| getPrice avg_price

                                    else
                                        Element.none
              }
            , { header = small_header "Qty."
              , width = fillPortion 1
              , view =
                    \{ quantity } ->
                        Element.el
                            [ centerY ]
                        <|
                            if getQuantity quantity == 0 then
                                case context of
                                    ShopItems ->
                                        text "OUT"

                                    InventoryItems ->
                                        text "NONE"

                                    CharacterItems ->
                                        text "NONE"

                            else
                                text <| "x" ++ (String.fromInt <| getQuantity quantity)
              }
            , { header = small_header "Item Type"
              , width = fillPortion 2
              , view =
                    \{ item } ->
                        Element.el
                            [ centerY ]
                        <|
                            render_item_type shop_trends item.item_type
              }
            , { header = small_header "Item Desc."
              , width = fillPortion 3
              , view =
                    \{ item } -> el [ centerY ] <| text <| clipText item.description 24
              }
            , { header = small_header "Controls"
              , width = fillPortion 1
              , view = controls_column
              }
            ]
                |> List.map
                    (\col ->
                        { col
                            | view =
                                \item ->
                                    el
                                        (if getQuantity item.quantity == 0 then
                                            [ cssRule "opacity" "0.5", height fill ]

                                         else
                                            [ cssRule "opacity" "1.0", height fill ]
                                        )
                                    <|
                                        el [ height fill, centerY ] <|
                                            col.view item
                        }
                    )
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
            ++ rendered_inventory_controls
            ++ (if not is_shop_context && List.length character.action_log > 0 then
                    divider

                else
                    []
               )
            ++ [ Element.table [ spacing 5 ] { data = items, columns = table_columns } ]


sort_by_bool_true_first : Bool -> Int
sort_by_bool_true_first bool =
    if bool then
        1

    else
        0


sort_by_bool_true_last : Bool -> Int
sort_by_bool_true_last bool =
    if bool then
        0

    else
        1


sort_func : InventoryRecord -> String
sort_func =
    .item >> .name


exclude_player_and_shop : { a | player_id : CharacterId, shop_id : CharacterId } -> List Character -> List Character
exclude_player_and_shop { player_id, shop_id } characters =
    List.filter
        (\c -> c.char_id /= player_id && c.char_id /= shop_id)
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

        render_tooltip plane item =
            let
                ( id, it_val ) =
                    CI.getData item

                ( item_type_, val ) =
                    it_val

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


getCharacter : Characters -> CharacterId -> Maybe Character
getCharacter characters char_id =
    characters
        |> List.filter (.char_id >> (==) char_id)
        |> List.head


getPlayer : Model -> Maybe Character
getPlayer { characters, player_id } =
    getCharacter characters player_id


getShop : Model -> Maybe Character
getShop { characters, shop_id } =
    getCharacter characters shop_id


withCharacter : Character -> Model -> Model
withCharacter new_char model =
    let
        new_characters =
            List.map
                (\char ->
                    if char.char_id == new_char.char_id then
                        new_char

                    else
                        char
                )
                model.characters
    in
    { model | characters = new_characters }


render_single_player_action_log : ItemDb -> PlayerActionLog -> Element Msg
render_single_player_action_log item_db player_action_log =
    paragraph []
        [ case player_action_log of
            TookSpecialActionInviteTrader ->
                text "Invited Trader"

            TookSpecialActionTriggerEvent special_event ->
                case special_event of
                    EventVeryDesiredItemType mb_item_type ->
                        let
                            _ =
                                Debug.log "mb_item_type" mb_item_type
                        in
                        text <|
                            ((Maybe.withDefault "Unknown" <| Maybe.map item_type_to_pretty_string mb_item_type)
                                ++ " -- These became quite valuable."
                            )

                    EventLeastDesiredItemType mb_item_type ->
                        text <|
                            ((Maybe.withDefault "Unknown" <| Maybe.map item_type_to_pretty_string mb_item_type)
                                ++ " -- Nobody is interested in these anymore."
                            )

            TookSpecialActionTogglePauseAi ->
                text "Toggle Play/Pause"

            TookSpecialActionUnlockItem item_id ->
                text <| "Found an item: " ++ (lookup_item_id_default item_db item_id).name
        ]


render_single_player_upgrade : PlayerUpgrade -> Element Msg
render_single_player_upgrade player_upgrade =
    case player_upgrade of
        AutomaticGPM gpm ->
            paragraph [] [ text "Income: ", render_gp gpm, text "/sec" ]


player_upgrades_display : List PlayerUpgrade -> Element Msg
player_upgrades_display player_upgrades =
    column [ height fill ]
        ([ el [ font_scaled 2, border_bottom 2, alignTop ] <| text "Upgrades" ]
            ++ [ column [ paddingXY 0 10 ] <| List.map render_single_player_upgrade player_upgrades ]
        )


player_action_log_display : ItemDb -> List PlayerActionLog -> Element Msg
player_action_log_display item_db player_action_logs =
    if List.length player_action_logs > 0 then
        column [ height fill ]
            ([ el [ font_scaled 2, border_bottom 2, alignTop ] <| text "Action Log" ]
                ++ [ column [ paddingXY 0 10, Element.spacing 4 ]
                        (player_action_logs
                            |> List.reverse
                            |> List.take 5
                            |> List.map (render_single_player_action_log item_db)
                        )
                   ]
            )

    else
        column [ paddingXY 0 10, spacing 5 ]
            ([ el [ font_scaled 2, border_bottom 2 ] <| text "Action Log" ]
                ++ [ text "Welcome!" ]
            )


showHideDebugInventoriesButton : Bool -> Element Msg
showHideDebugInventoriesButton show_debug_inventories =
    let
        buttonText =
            if show_debug_inventories then
                "Hide Debug"

            else
                "Show Debug"
    in
    danger_button [ defineHtmlId "show_debug_inventories" ]
        ToggleShowDebugInventories
        buttonText


view_shop_tab_type : Model -> Element Msg
view_shop_tab_type model =
    let
        welcome_header =
            Element.el [ font_scaled 3, padding_bottom 10 ] <| text "Welcome to the Item Shop!"

        debug_inventories : List (Element Msg)
        debug_inventories =
            exclude_player_and_shop model model.characters
                |> List.sortBy (.char_id >> UUID.toString)
                |> List.map
                    (\character ->
                        Element.Keyed.el [ height fill, paddingXY 0 10, width fill ]
                            ( UUID.toString character.char_id
                            , render_inventory_grid
                                model
                                (character.name ++ "'s Inventory")
                                character
                                model.shop_trends
                                model.hovered_item_in_character
                                CharacterItems
                                (always Element.none)
                            )
                    )

        maybe_shop : Maybe Character
        maybe_shop =
            getShop model

        maybe_player : Maybe Character
        maybe_player =
            getPlayer model

        paused_border_attrs =
            [ Border.color color_light_grey, Border.width 10, Border.dashed ]

        unpaused_border_attrs =
            [ Border.color color_white, Border.width 10, Border.dashed ]
    in
    Element.el
        (padding 10
            :: (if model.ai_updates_paused then
                    paused_border_attrs

                else
                    unpaused_border_attrs
               )
        )
    <|
        Element.column
            [ width fill, font_scaled 1, height fill ]
        <|
            [ welcome_header
            , row [ spacing 5 ]
                [ Element.link []
                    { url = "#items"
                    , label =
                        secondary_button [] (ChangeTabType ItemsUnlockedTabType) "View Items"
                    }
                , outline_button [] ToggleShowMainChart <|
                    if model.show_main_chart then
                        "Hide Charts"

                    else
                        "Charts"
                ]
            , if model.show_main_chart then
                Element.el [ paddingXY 0 10, width fill ] <| charts_display model.historical_shop_trends model.hovered_trend_chart

              else
                Element.none
            , row [ width fill, height <| Element.px 10 ] []
            , row [ width fill, spacingXY 10 0 ]
                [ el [ width <| fillPortion 3, alignTop ] <| Lazy.lazy2 player_action_log_display model.item_db model.historical_player_actions
                , el [ width <| fillPortion 7, alignTop ] <| Lazy.lazy player_upgrades_display model.player_upgrades
                ]
            , case maybe_player of
                Just player ->
                    special_actions_display model.player_upgrades model.hovered_tooltip player model.ai_updates_paused

                Nothing ->
                    Element.none
            , trends_display
                model.shiftIsPressed
                model.item_db
                model.shop_trends
                model.characters
                model.shop_trends_hovered
            , Element.el [ paddingXY 0 0, width fill ] <|
                case maybe_shop of
                    Just shop ->
                        render_inventory_grid
                            model
                            "Items For Sale"
                            shop
                            model.shop_trends
                            model.hovered_item_in_character
                            ShopItems
                            (\{ item, quantity, avg_price } ->
                                shop_buy_button
                                    (get_single_adjusted_item_cost model.shop_trends item)
                                    (case maybe_player of
                                        Just player ->
                                            player.held_gold

                                        Nothing ->
                                            99999
                                    )
                                    { item = item, quantity = quantity, avg_price = avg_price }
                            )

                    Nothing ->
                        el [ Font.color <| rgb 1 0 0, font_scaled 5 ] <| text "ERR: Can't find shop"
            , Element.el [ paddingXY 0 10, width fill ] <|
                case maybe_player of
                    Just player ->
                        render_inventory_grid
                            model
                            "Items In Inventory"
                            player
                            model.shop_trends
                            model.hovered_item_in_character
                            InventoryItems
                            (\{ item, quantity, avg_price } ->
                                shop_sell_button
                                    (getQuantity quantity >= 1)
                                    { item = item, quantity = setQuantity 1, avg_price = avg_price }
                            )

                    Nothing ->
                        text "Can't find player"
            ]
                ++ [ column [ width fill ] <|
                        showHideDebugInventoriesButton model.show_debug_inventories
                            :: (if model.show_debug_inventories then
                                    debug_inventories

                                else
                                    []
                               )
                   ]


render_unlocked_item : ItemDbRecord -> Element Msg
render_unlocked_item { item, trade_stats, is_unlocked } =
    column [ width fill, height fill ]
        [ text <| item.name
        , row [ Font.size 12 ]
            [ if is_unlocked then
                Element.none

              else
                el [ Font.color color_primary ] <| text "LOCKED"
            ]
        , row [ width (fill |> Element.maximum 200), Font.size 14, spacingXY 10 0 ]
            [ item_type_to_pretty_string item.item_type
                |> text
            , item.raw_gold_cost
                |> render_gp
                |> el [ alignRight ]
            ]
        , row [ Font.size 12 ]
            [ text "Num Bought: "
            , trade_stats
                |> .times_you_bought
                |> String.fromInt
                |> text
            ]
        , row [ Font.size 12 ]
            [ text "Num Sold: "
            , trade_stats
                |> .times_you_sold
                |> String.fromInt
                |> text
            ]
        , row [ Font.size 12 ]
            [ text "Others' Trades: "
            , trade_stats
                |> .times_others_traded
                |> String.fromInt
                |> text
            ]
        ]


view_items_unlocked_tab_type : ItemDb -> Element Msg
view_items_unlocked_tab_type item_db =
    let
        back_btn =
            Element.link []
                { url = "#shop"
                , label = danger_button [] (ChangeTabType ShopTabType) "Back to Shop"
                }

        item_grid : Element Msg
        item_grid =
            Dict.values item_db
                |> List.sortBy (.is_unlocked >> sort_by_bool_true_last)
                |> List.map render_unlocked_item
                |> Element.wrappedRow [ width fill, spacing 20 ]
    in
    Debug.log "render view_items_unlocked_tab_type" <|
        column [ spacing 10 ] [ text "Item Codex", back_btn, item_grid ]


cssRule : String -> String -> Element.Attribute Msg
cssRule name value =
    Html.Attributes.style name value |> Element.htmlAttribute


defineHtmlId : String -> Element.Attribute Msg
defineHtmlId name =
    Html.Attributes.id name |> Element.htmlAttribute


noUserSelect : Element.Attribute Msg
noUserSelect =
    Html.Attributes.style "userSelect" "none" |> Element.htmlAttribute


pointerEventsNone : Element.Attribute Msg
pointerEventsNone =
    Html.Attributes.style "pointer-events" "none" |> Element.htmlAttribute


pointerEventsAll : Element.Attribute Msg
pointerEventsAll =
    Html.Attributes.style "pointer-events" "all" |> Element.htmlAttribute


viewOverlay : Model -> Element Msg
viewOverlay model =
    model
        |> getPlayer
        |> Maybe.map
            (\player ->
                el [ width fill, height fill, Font.size 12, pointerEventsNone, padding 1 ] <|
                    el
                        [ Font.alignRight
                        , Element.alignRight
                        , Element.alignBottom
                        , Background.color color_white
                        , Border.color color_ultra_light_grey
                        , Border.width 1
                        , Border.rounded 3
                        , pointerEventsAll
                        , padding 10
                        ]
                    <|
                        row [ noUserSelect ]
                            [ text "Gold: "
                            , render_gp <| player.held_gold
                            ]
            )
        |> Maybe.withDefault Element.none


view : Model -> Html.Html Msg
view model =
    Element.layoutWith
        { options =
            [ Element.noStaticStyleSheet
            , Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Element.htmlAttribute <| Html.Attributes.id "itemshop"
        , Element.inFront <| viewOverlay model
        , Element.htmlAttribute <| Html.Events.on "wheel" (Decode.succeed ScrollViewport)
        ]
    <|
        case model.tab_type of
            ShopTabType ->
                Lazy.lazy view_shop_tab_type model

            ItemsUnlockedTabType ->
                Lazy.lazy view_items_unlocked_tab_type model.item_db


scaled : Int -> Int
scaled val =
    modular 14 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale


hoveredTooltipMatchesId : HoveredTooltip -> String -> Bool
hoveredTooltipMatchesId hovered_tooltip tooltip_id =
    case hovered_tooltip of
        HoveredTooltipWithoutOffset tooltip_data ->
            if tooltip_data.hovered_tooltip_id == tooltip_id then
                True

            else
                False

        HoveredTooltipWithOffset hovered_tooltip_data ->
            if hovered_tooltip_data.hovered_tooltip_id == tooltip_id then
                True

            else
                False

        NoHoveredTooltip ->
            False


primary_button_tooltip :
    List (Element.Attribute Msg)
    -> Msg
    -> String
    -> TooltipConfig
    -> HoveredTooltip
    -> Element Msg
primary_button_tooltip custom_attrs on_press label { tooltip_id, tooltip_text } hovered_tooltip =
    let
        { offset_x, offset_y } =
            case hovered_tooltip of
                HoveredTooltipWithOffset data ->
                    data

                HoveredTooltipWithoutOffset cached_data ->
                    cached_data

                _ ->
                    { offset_x = 0, offset_y = 0, hovered_tooltip_id = "UNUSED" }

        tooltip_el =
            Element.el
                [ width Element.shrink
                , Font.color <| rgb 0 0 0
                , Background.color <| rgb 1 1 1
                , Border.color <| rgb 0.35 0.35 0.35
                , Border.rounded 3
                , Border.width 2
                , padding 10
                , if offset_y == 0 then
                    Element.moveUp 20

                  else
                    Element.moveDown offset_y
                , Element.moveRight offset_x
                , centerX
                , Element.htmlAttribute <|
                    Html.Attributes.id ("tooltip__" ++ tooltip_id)
                ]
            <|
                text tooltip_text

        offset_w =
            target offsetWidth

        tooltip_attr =
            if hoveredTooltipMatchesId hovered_tooltip tooltip_id then
                [ Element.above tooltip_el ]

            else
                []
    in
    primary_button
        ([ Events.onMouseLeave <| EndTooltipHover tooltip_id
         , Events.onMouseEnter <| StartTooltipHover tooltip_id
         ]
            ++ tooltip_attr
            ++ custom_attrs
        )
        on_press
        label


buildTooltipConfig : String -> TooltipConfig
buildTooltipConfig text =
    { tooltip_id = UUID.forName text UUID.dnsNamespace |> UUID.toString
    , tooltip_text = text
    }


build_special_action_button : HoveredTooltip -> Character -> SpecialAction -> String -> String -> Price -> Element Msg
build_special_action_button hovered_tooltip character special_action title tooltip_text price =
    let
        is_disabled =
            case price of
                Free ->
                    False

                Price cost ->
                    if character.held_gold >= cost then
                        False

                    else
                        True

        tooltip_config =
            tooltip_text
                |> (\t ->
                        if price /= Free then
                            t ++ "\n\nCosts " ++ (render_gp_string <| getPrice price)

                        else
                            t
                   )
                |> buildTooltipConfig

        button_attrs =
            if is_disabled then
                [ Background.color color_grey
                , Border.color color_grey
                ]
                    ++ (if hoveredTooltipMatchesId hovered_tooltip tooltip_config.tooltip_id then
                            [ Background.color <| rgb 0 0 0
                            , Border.color <| rgb 0 0 0

                            --required mouseOver to override the default buttons behaviour
                            , Element.mouseOver [ Background.color color_light_grey, Border.color color_light_grey ]
                            ]

                        else
                            []
                       )

            else
                []

        msg =
            if not is_disabled then
                OnSpecialAction special_action price

            else
                Noop
    in
    primary_button_tooltip button_attrs
        msg
        title
        tooltip_config
        hovered_tooltip


scale_increase_income_cost : Int -> Price
scale_increase_income_cost current_level =
    (20 + (5 * current_level * current_level) * 2) |> setPrice


special_actions_display : List PlayerUpgrade -> HoveredTooltip -> Character -> Bool -> Element Msg
special_actions_display player_upgrades hovered_tooltip player ai_updates_paused =
    let
        button_toggle_ai_pause =
            build_special_action_button
                hovered_tooltip
                player
                TogglePauseAi
                (if ai_updates_paused then
                    "Resume Time"

                 else
                    "Pause Time"
                )
                "You tap your medallion, and time comes to a halt.\n\nYou take a breath, and feel a weight off your shoulders. You'll take your time with things."
                Free

        button_search =
            build_special_action_button
                hovered_tooltip
                player
                InviteTrader
                "Invite Trader"
                "Invite a fellow Trader.\n\nThey may or may not have new wares you've never seen!"
                (setPrice 50)

        button_high_desire =
            build_special_action_button
                hovered_tooltip
                player
                (TriggerEvent (EventVeryDesiredItemType Nothing))
                "Spread Good Rumour"
                "Sets a random Item Type to high value.\n\nSpreads a rumour that a given Item Type was the talk of the next town over."
                Free

        button_low_desire =
            build_special_action_button
                hovered_tooltip
                player
                (TriggerEvent (EventLeastDesiredItemType Nothing))
                "Spread Bad Rumour"
                "Sets a random Item Type to low value.\n\nSpreads a rumour that a given Item Type has a surplus of sellers."
                Free

        button_unlock_item =
            build_special_action_button
                hovered_tooltip
                player
                UnlockItem
                "Item Search"
                "Spend cash to hire a mercenary to seek out items.\n\nAllows for invited traders to have new items."
                (setPrice 25)

        button_increase_income =
            let
                income_level =
                    List.foldl
                        (\u acc ->
                            case u of
                                AutomaticGPM lvl ->
                                    lvl
                        )
                        1
                        player_upgrades
            in
            build_special_action_button
                hovered_tooltip
                player
                IncreaseIncome
                "Invest"
                "Invest in another business, earning more income.\n\nIncreases the gold you get per second."
                (scale_increase_income_cost income_level)
    in
    column [ width fill, spacing 10, paddingXY 0 10 ]
        [ el [ font_scaled 2, border_bottom 2 ] <| text "Special Actions"
        , row [ width fill, spacingXY 10 0 ]
            [ button_toggle_ai_pause
            , button_increase_income
            , button_search
            , button_unlock_item
            , button_high_desire
            , button_low_desire
            ]
        ]


natural =
    Fuzz.intRange 0 Random.maxInt


positive =
    Fuzz.intRange 0 Random.maxInt


suite : Test
suite =
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "root test suite"
        [ describe "Basic math check for changing averages"
            [ test "Adding nothing changes nothing in average" <|
                \_ ->
                    let
                        orig_avg =
                            10
                    in
                    Expect.equal orig_avg (add_to_average orig_avg 1 0 0)
            , fuzz natural "Starting the average from nothing is just the number you add" <|
                \val ->
                    Expect.equal val (add_to_average 0 0 val 1)
            , test "Adding a single item works to change the average" <|
                \_ ->
                    let
                        orig_avg =
                            10

                        orig_num =
                            1
                    in
                    Expect.equal 15 (add_to_average orig_avg orig_num 20 1)
            , test "Removing nothing changes nothing in average" <|
                \_ ->
                    let
                        orig_avg =
                            10
                    in
                    Expect.equal orig_avg (sub_from_average orig_avg 1 0 0)
            , test "Removing a single item changes the average as expected" <|
                \_ ->
                    let
                        orig_avg =
                            20

                        orig_num =
                            2
                    in
                    Expect.equal 30 (sub_from_average orig_avg orig_num 10 1)
            , test "Removing a single item at the same price doesnt change anything" <|
                \_ ->
                    let
                        orig_avg =
                            20

                        orig_num =
                            2
                    in
                    Expect.equal 30 (sub_from_average orig_avg orig_num 10 1)
            ]
        , describe "Item Shop stuff" <|
            let
                test_item_db : ItemDb
                test_item_db =
                    initial_item_db

                test_shop_trends : ShopTrends
                test_shop_trends =
                    initial_shop_trends

                test_character : Character
                test_character =
                    create_character (generate_uuid "Test Character !!") "Testy McTested"

                test_character2 : Character
                test_character2 =
                    create_character (generate_uuid "Second test character") "Testa Mysticles"

                test_model : Model
                test_model =
                    init "" |> Tuple.first

                ( test_item, test_item_qty, test_avg_price ) =
                    ( lookup_item_id_str_default test_item_db "a41ae9d3-61f0-54f9-800e-56f53ed3ac98", Quantity 12, setPrice 9999 )

                ( test_item2, test_item_qty2, test_avg_price2 ) =
                    ( lookup_item_id_str_default test_item_db "c3c38323-1743-5a47-a8e3-bf6ec28137f9", Quantity 12, setPrice 9999 )
            in
            [ test "clipText test clips" <|
                \_ ->
                    Expect.equal "abc..." <| clipText "abcdef" 3
            , test "clipText test doesnt clip" <|
                \_ ->
                    Expect.equal "abcdef" <| clipText "abcdef" 30
            , test "getting adjusted item cost should return exact item cost" <|
                \_ ->
                    Expect.equal
                        test_item.raw_gold_cost
                        (get_single_adjusted_item_cost
                            test_shop_trends
                            test_item
                        )
            , test "getting adjusted item cost should return double item cost at 200% markup" <|
                \_ ->
                    let
                        { item_type_sentiment } =
                            test_shop_trends

                        { item_type } =
                            test_item

                        item_type_id =
                            item_type_to_id item_type

                        new_test_shop_trends =
                            { test_shop_trends
                                | item_type_sentiment = Dict.update item_type_id (always <| Just 2.0) item_type_sentiment
                            }
                    in
                    Expect.equal
                        (test_item.raw_gold_cost * 2)
                        (get_single_adjusted_item_cost
                            new_test_shop_trends
                            test_item
                        )
            , test "test making sure someone can afford something" <|
                \_ ->
                    check_can_afford_one { test_character | held_gold = 99999 } test_shop_trends test_item
                        |> Expect.true "Expected to be able to afford item"
            , test "test making sure someone cannot afford something with 0 bucks" <|
                \_ ->
                    check_can_afford_one { test_character | held_gold = 0 } test_shop_trends test_item
                        |> Expect.false "Expected to be not be able to afford item without any held gold"
            , test "test check_nonzero_desire" <|
                \_ ->
                    check_nonzero_desire
                        { test_character
                            | item_types_desired =
                                Dict.update
                                    (item_type_to_id test_item.item_type)
                                    (always <| Just 1.0)
                                    test_character.item_types_desired
                        }
                        test_item
                        |> Expect.true "Expected a nonzero desire"
            , test "test NOT check_nonzero_desire" <|
                \_ ->
                    check_nonzero_desire
                        { test_character
                            | item_types_desired =
                                Dict.update
                                    (item_type_to_id test_item.item_type)
                                    (always <| Just 0.0)
                                    test_character.item_types_desired
                        }
                        test_item
                        |> Expect.false "Expected a desire of zero, so it should be false"
            , test "test adding a totally new item to inventory records adds it" <|
                \_ ->
                    let
                        new_item =
                            { name = "Non-existant item"
                            , item_type = Armor
                            , raw_gold_cost = 60
                            , description = "Used for testing"
                            , id = UUID.forName "non-existant item" UUID.dnsNamespace
                            }

                        orig_len =
                            List.length test_character.held_items
                    in
                    List.length
                        (add_item_to_inventory_records
                            test_character.held_items
                            new_item
                            (setQuantity 1)
                            new_item.raw_gold_cost
                        )
                        |> Expect.notEqual orig_len
            , test "make sure a special actions cost is removed from the player" <|
                \_ ->
                    let
                        model =
                            test_model
                    in
                    model
                        |> getPlayer
                        |> (\mb_player ->
                                case mb_player of
                                    Just orig_player ->
                                        update_special_action InviteTrader (setPrice 10) test_model
                                            |> (\( new_model, _ ) ->
                                                    case getPlayer new_model of
                                                        Just new_player ->
                                                            Expect.equal (orig_player.held_gold - 10) <| new_player.held_gold

                                                        Nothing ->
                                                            Expect.fail "A player should be present in the model characters after a special action"
                                               )

                                    Nothing ->
                                        Expect.fail "a player should exist in the initial model"
                           )
            , test "test adding an existing item to inventory records updates the qty instead of appending a new item" <|
                \_ ->
                    let
                        new_test_character =
                            { test_character
                                | held_items = { item = test_item, quantity = setQuantity 1, avg_price = setPrice 9999 } :: test_character.held_items
                            }

                        orig_len =
                            List.length new_test_character.held_items

                        updated_records =
                            add_item_to_inventory_records
                                new_test_character.held_items
                                test_item
                                (setQuantity 1)
                                test_item.raw_gold_cost

                        many_updated_record : InventoryRecords
                        many_updated_record =
                            List.filter
                                (.item
                                    >> .item_type
                                    >> (==) test_item.item_type
                                )
                                updated_records
                    in
                    updated_records
                        |> Expect.all
                            [ Expect.equal orig_len << List.length
                            , Expect.true "exists exactly one record"
                                << always
                                    (List.length many_updated_record == 1)
                            , Expect.equal 2
                                << always
                                    (List.head many_updated_record
                                        |> Maybe.map (.quantity >> getQuantity)
                                        |> Maybe.withDefault 0
                                    )
                            ]
            , fuzz (Fuzz.map Time.millisToPosix int) "SpecialAction: UnlockItem unlocks an item" <|
                \ai_tick_time ->
                    let
                        item_db : ItemDb
                        item_db =
                            Dict.fromList [ create_db_entry ( False, test_item ) ]

                        model =
                            { test_model | item_db = item_db, ai_tick_time = ai_tick_time }

                        updated_model =
                            special_action_unlock_item model

                        updated_item_db =
                            updated_model.item_db
                    in
                    Expect.true "contains an unlocked itemdbrecord, since we just asked to unlock it" <|
                        List.all (\item_db_record -> item_db_record.is_unlocked) <|
                            Dict.values updated_item_db
            , fuzz (Fuzz.map Time.millisToPosix int) "SpecialAction: UnlockItem unlocks only a single item" <|
                \ai_tick_time ->
                    let
                        item_db : ItemDb
                        item_db =
                            Dict.fromList [ create_db_entry ( False, test_item ), create_db_entry ( False, test_item2 ) ]

                        model =
                            { test_model | item_db = item_db, ai_tick_time = ai_tick_time }

                        updated_model =
                            special_action_unlock_item model

                        updated_item_db =
                            updated_model.item_db
                    in
                    updated_item_db
                        |> Dict.values
                        |> List.filter (\item_db_record -> item_db_record.is_unlocked)
                        |> List.length
                        |> (==) 1
                        |> Expect.true "contains an unlocked itemdbrecord, since we just asked to unlock it"
            , test "AutomaticGPM works with 1 level" <|
                \_ ->
                    case getPlayer test_model of
                        Just player ->
                            update_player test_model
                                |> (\m ->
                                        case getPlayer m of
                                            Just updated_player ->
                                                Expect.equal (player.held_gold + 1) updated_player.held_gold

                                            Nothing ->
                                                Expect.fail "Couldnt find updated player"
                                   )

                        Nothing ->
                            Expect.fail "Couldn't find player"
            , fuzz (tuple ( int, Fuzz.intRange 1 Random.maxInt )) "removing all quantities leaves avg cost of 0" <|
                \( qtyToAdd, totalCost ) ->
                    let
                        item =
                            test_item

                        qty =
                            setQuantity qtyToAdd

                        total_cost =
                            totalCost

                        origItems =
                            add_item_to_inventory_records
                                test_character.held_items
                                item
                                qty
                                test_item.raw_gold_cost

                        newItems =
                            remove_item_from_inventory_records
                                origItems
                                item
                                qty
                                total_cost

                        item_finder =
                            find_matching_records item

                        origRecord =
                            List.head <| List.filter item_finder origItems

                        newRecord =
                            List.head <| List.filter item_finder newItems
                    in
                    -- Expect.equal origItems newItems
                    case
                        Maybe.map2
                            (\orig new ->
                                getPrice new.avg_price
                                    |> Expect.all
                                        -- TODO: figure out the correct handling for what the new avg should be (should it ever be 0?)
                                        -- [ if qtyToAdd /= 0 then
                                        --     Expect.notEqual (getPrice orig.avg_price)
                                        --
                                        --   else
                                        --     Expect.true "" << (always True)
                                        -- , Expect.equal 0
                                        [ Expect.equal 0
                                        ]
                            )
                            origRecord
                            newRecord
                    of
                        Nothing ->
                            Expect.fail "Idk"

                        Just expectation ->
                            expectation
            , test "AutomaticGPM works with 10 levels" <|
                \_ ->
                    case getPlayer test_model of
                        Just player ->
                            update_player { test_model | player_upgrades = [ AutomaticGPM 10 ] }
                                |> (\m ->
                                        case getPlayer m of
                                            Just updated_player ->
                                                Expect.equal (player.held_gold + 10) updated_player.held_gold

                                            Nothing ->
                                                Expect.fail "Couldnt find updated player"
                                   )

                        Nothing ->
                            Expect.fail "Couldn't find player"
            , fuzz int "AutomaticGPM doesn't go past 50" <|
                \to_add ->
                    case getPlayer test_model of
                        Just player ->
                            update_player { test_model | player_upgrades = [ AutomaticGPM to_add ] }
                                |> (\m ->
                                        case getPlayer m of
                                            Just updated_player ->
                                                Expect.true
                                                    "Can't have more than 50 max gold"
                                                    (updated_player.held_gold <= 50)

                                            Nothing ->
                                                Expect.fail "Couldnt find updated player"
                                   )

                        Nothing ->
                            Expect.fail "Couldn't find player"
            , test "scaling IncreaseIncome upgrade with level 2" <|
                \_ ->
                    Expect.equal (setPrice 60) <| scale_increase_income_cost 2
            , test "scaling IncreaseIncome upgrade with level 3" <|
                \_ ->
                    Expect.equal (setPrice 110) <| scale_increase_income_cost 3
            , test "scaling IncreaseIncome upgrade with level 4" <|
                \_ ->
                    Expect.equal (setPrice 180) <| scale_increase_income_cost 4
            , test "scaling IncreaseIncome upgrade with level 5" <|
                \_ ->
                    Expect.equal (setPrice 270) <| scale_increase_income_cost 5
            ]
        ]
