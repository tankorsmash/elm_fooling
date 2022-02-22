module ItemShop exposing (Model, Msg, add_to_average, init, setDevice, sub_from_average, subscriptions, suite, update, view)

import Animator
import Array
import Battle
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
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
import Interface as UI exposing (ColorTheme(..), defaultRounded)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra exposing (maybe)
import List.Extra
import Process
import Random
import Random.List
import Sfxr
import Task
import Test exposing (..)
import Time
import Tuple3
import UUID exposing (UUID)
import Url


type SortDirection
    = Ascending
    | Descending


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


encodeTradeParty : TradeParty -> Value
encodeTradeParty trade_party =
    Encode.string <|
        case trade_party of
            ShopParty ->
                "ShopParty"

            PlayerParty ->
                "PlayerParty"

            CharacterParty char_id ->
                "CharacterParty__" ++ UUID.toString char_id


decodeTradeParty : Decoder TradeParty
decodeTradeParty =
    Decode.map
        (\str ->
            case str of
                "ShopParty" ->
                    Just ShopParty

                "PlayerParty" ->
                    Just PlayerParty

                unknown ->
                    case String.split "CharacterParty__" unknown of
                        _ :: uuid :: rest ->
                            case UUID.fromString uuid of
                                Ok char_id ->
                                    Just <| CharacterParty char_id

                                Err _ ->
                                    Nothing

                        anythingelse ->
                            Nothing
        )
        Decode.string
        |> Decode.andThen
            (\maybe_trade_party ->
                case maybe_trade_party of
                    Just trade_party ->
                        Decode.succeed trade_party

                    Nothing ->
                        Decode.fail "not a valid trade party"
            )


trade_party_to_str : Characters -> TradeParty -> String
trade_party_to_str (Characters { player, shop, others }) party =
    case party of
        ShopParty ->
            "Shop"

        PlayerParty ->
            "Player"

        CharacterParty char_id ->
            case
                List.head <|
                    List.filter (.char_id >> (==) char_id) others
            of
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


encodeSpecialEvent : SpecialEvent -> Encode.Value
encodeSpecialEvent specialEvent =
    case specialEvent of
        EventVeryDesiredItemType maybeItemType ->
            Encode.object
                [ ( "type", Encode.string "EventVeryDesiredItemType" )
                , ( "maybeItemType", maybe encodeItemType maybeItemType )
                ]

        EventLeastDesiredItemType maybeItemType ->
            Encode.object
                [ ( "type", Encode.string "EventLeastDesiredItemType" )
                , ( "maybeItemType", maybe encodeItemType maybeItemType )
                ]


type SpecialAction
    = InviteTrader
    | Mine
    | TriggerEvent SpecialEvent
    | TogglePauseAi
    | UnlockItem
    | IncreaseIncome
    | IncreaseBPtoSP
    | CommunityFund


type UiOptionMsg
    = MouseEnterShopItem ListContext ( CharacterId, Item )
    | MouseLeaveShopItem ListContext ( CharacterId, Item )
    | GotTooltipMsg UI.TooltipMsg
    | GotTooltipSize (Result Browser.Dom.Error Browser.Dom.Element)
    | ScrollViewport
    | GotViewport Browser.Dom.Viewport
    | GotShowDebugElement (Result Browser.Dom.Error Browser.Dom.Element)
    | ChangeInventorySortType InventorySortType
    | StartTrendsHover
    | EndTrendsHover
    | CycleFilterDisplayedItemsForward CharacterId (Maybe ItemType)
    | CycleFilterDisplayedItemsBackward CharacterId (Maybe ItemType)
    | ToggleHideNonZeroRows CharacterId
    | ToggleShowMainChart
      -- | OnTrendChartHover (List (CI.One TrendSnapshot CI.Dot))
    | OnTrendChartHover (List (CI.One TrendChartDatum CI.Dot))
    | ToggleShowDebugInventories


type Msg
    = Noop
    | PlayerBuyItemFromShop Item Quantity
    | PlayerSellItemToShop Item Quantity
    | TickSecond Time.Posix
    | ForceTickSecond
    | KeyPressedMsg KeyEventMsg
    | KeyReleasedMsg KeyEventMsg
    | OnSpecialAction SpecialAction Price
    | ChangeTabType TabType
    | SacrificeItem Item
    | ToggleColorTheme
    | GotBattleMsg Battle.Msg
    | GotUiOptionsMsg UiOptionMsg
    | ChangeCurrentPhase TimePhase
      -- BeginDay just sets up the ActivePhase with the current time etc
    | BeginDay
      -- EndDay triggers onPrepNewDay
    | EndDay
    | CashInQuestType QuestData
    | ToggleViewGemUnlocksInPostPhase
    | UnlockProgressUnlock ProgressUnlock Price
    | RuntimeTriggeredAnimationStep Time.Posix
    | ClickedTitleTextLabel
    | ClickedTitlePlayLabel


type TitleScreenAnimationState
    = HighTitle
    | LowTitle


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


minQuantity : Quantity -> Quantity -> Quantity
minQuantity qty other_qty =
    case qty of
        Quantity orig_val ->
            Quantity <| min orig_val (getQuantity other_qty)


maxQuantity : Quantity -> Quantity -> Quantity
maxQuantity qty other_qty =
    case qty of
        Quantity orig_val ->
            Quantity <| max orig_val (getQuantity other_qty)


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


encodeShopTrends : ShopTrends -> Decode.Value
encodeShopTrends { item_type_sentiment, item_trade_logs } =
    Encode.object
        [ ( "item_type_sentiment", encodeItemSentiments item_type_sentiment )
        , ( "item_trade_logs", Encode.list encodeItemTradeLog item_trade_logs )
        ]


decodeShopTrends : Decoder ShopTrends
decodeShopTrends =
    Decode.map2 ShopTrends
        (field "item_type_sentiment" <| decodeItemSentiments "invalid shop trends sentiment")
        (field "item_trade_logs" <| Decode.list decodeItemTradeLog)


type WantedAction
    = WantedToBuy
    | WantedToSell


encodeWantedAction : WantedAction -> Decode.Value
encodeWantedAction wanted_action =
    case wanted_action of
        WantedToBuy ->
            Encode.string "WantedToBuy"

        WantedToSell ->
            Encode.string "WantedToSell"


decodeWantedAction : Decoder WantedAction
decodeWantedAction =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "WantedToBuy" ->
                        Decode.succeed WantedToBuy

                    "WantedToSell" ->
                        Decode.succeed WantedToSell

                    _ ->
                        Decode.fail "invalid wanted action"
            )


type ActionLogType
    = Traded ItemTradeLog
    | WantedButCouldntTrade WantedAction
    | FetchedItem ItemId
    | FetchedItemButFundNotBigEnough ItemId
    | DidNothing


encodeActionLogType : ActionLogType -> Decode.Value
encodeActionLogType action_log_type =
    case action_log_type of
        Traded item_trade_log ->
            Encode.list identity [ Encode.string "Traded", encodeItemTradeLog item_trade_log ]

        WantedButCouldntTrade wanted_action ->
            Encode.list identity [ Encode.string "WantedButCouldntTrade", encodeWantedAction wanted_action ]

        FetchedItem item_id ->
            Encode.list identity [ Encode.string "FetchedItem", encodeItemId item_id ]

        FetchedItemButFundNotBigEnough item_id ->
            Encode.list identity [ Encode.string "FetchedItemButFundNotBigEnough", encodeItemId item_id ]

        DidNothing ->
            Encode.list identity [ Encode.string "DidNothing" ]


decodeActionLogType : Decoder ActionLogType
decodeActionLogType =
    Decode.list Decode.string
        |> Decode.andThen
            (\all_action_log_type_str ->
                case all_action_log_type_str of
                    [] ->
                        Decode.fail "Not a valid ActionLogType"

                    single_action_log_type :: [] ->
                        case single_action_log_type of
                            "DidNothing" ->
                                Decode.succeed DidNothing

                            _ ->
                                Decode.fail "single action log type provided was invalid"

                    multiple_action_log_type ->
                        case multiple_action_log_type of
                            "WantedButCouldntTrade" :: wanted_action :: [] ->
                                Decode.map WantedButCouldntTrade decodeWantedAction

                            "Traded" :: item_trade_log :: [] ->
                                Decode.map Traded decodeItemTradeLog

                            "FetchedItem" :: item_id :: [] ->
                                Decode.map FetchedItem UUID.jsonDecoder

                            [] ->
                                Decode.fail <| "multiple action log type is empty, somehow: "

                            _ ->
                                Decode.fail "unrecognized multipart action log"
             -- Decode.succeed DidNothing
            )


type alias ActionLog =
    { log_type : ActionLogType
    , time : Time.Posix
    }


encodeActionLog : ActionLog -> Decode.Value
encodeActionLog { log_type, time } =
    Encode.object
        [ ( "log_type", encodeActionLogType log_type )
        , ( "time", Encode.int <| Time.posixToMillis time )
        ]


type alias ItemTradeLog =
    { item_id : ItemId
    , quantity : Quantity
    , gold_cost : Int
    , from_party : TradeParty
    , to_party : TradeParty
    }


encodeItemTradeLog : ItemTradeLog -> Decode.Value
encodeItemTradeLog item_trade_log =
    Encode.object
        [ ( "item_id", encodeItemId item_trade_log.item_id )
        , ( "quantity", Encode.int <| getQuantity item_trade_log.quantity )
        , ( "gold_cost", Encode.int item_trade_log.gold_cost )
        , ( "from_party", encodeTradeParty item_trade_log.from_party )
        , ( "to_party", encodeTradeParty item_trade_log.to_party )
        ]


decodeItemTradeLog : Decoder ItemTradeLog
decodeItemTradeLog =
    Decode.map5 ItemTradeLog
        (field "item_id" UUID.jsonDecoder)
        (field "quantity" decodeQuantity)
        (field "gold_cost" Decode.int)
        (field "from_party" decodeTradeParty)
        (field "to_party" decodeTradeParty)


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
    , held_blood : Int
    , held_gems : Int
    }


type Player
    = Player Character


getInnerPlayer : Player -> Character
getInnerPlayer (Player player) =
    player


type Shop
    = Shop Character


getInnerShop : Shop -> Character
getInnerShop (Shop shop) =
    shop


type Characters
    = Characters { player : Player, shop : Shop, others : List Character }


encodeCharacters : Characters -> Encode.Value
encodeCharacters (Characters { player, shop, others }) =
    Encode.object
        [ ( "player", encodeCharacter <| getInnerPlayer player )
        , ( "shop", encodeCharacter <| getInnerShop shop )
        , ( "others", Encode.list encodeCharacter others )
        ]


decodeCharacters : ItemDb -> Decoder Characters
decodeCharacters itemDb =
    Decode.map3
        (\p s o ->
            Characters { player = Player p, shop = Shop s, others = o }
        )
        (field "player" <| decodeCharacter itemDb)
        (field "shop" <| decodeCharacter itemDb)
        (field "others" <| Decode.list (decodeCharacter itemDb))


charactersToList : Characters -> List Character
charactersToList (Characters { player, shop, others }) =
    getInnerPlayer player :: getInnerShop shop :: others


mapCharacters : (Character -> Character) -> Characters -> Characters
mapCharacters mapFunc ((Characters { player, shop, others }) as characters) =
    let
        rawChars : List Character
        rawChars =
            characters
                |> charactersToList
                |> List.map mapFunc

        matchesPlayerId =
            charIdMatches (.char_id (getInnerPlayer player))

        matchesShopId =
            charIdMatches (.char_id (getInnerShop shop))

        newPlayer =
            rawChars
                |> List.filter matchesPlayerId
                |> List.head
                |> --FIXME this is hacky because we KNOW player and shop are present
                   Maybe.withDefault (getInnerPlayer player)
                |> Player

        newShop =
            rawChars
                |> List.filter matchesShopId
                |> List.head
                |> --FIXME this is hacky because we KNOW player and shop are present
                   Maybe.withDefault (getInnerShop shop)
                |> Shop

        newOthers =
            List.filter (\c -> not (matchesPlayerId c || matchesShopId c)) rawChars
    in
    Characters { player = newPlayer, shop = newShop, others = newOthers }


charactersLength : Characters -> Int
charactersLength (Characters { player, shop, others }) =
    2 + List.length others


addOther : Characters -> Character -> Characters
addOther (Characters { player, shop, others }) newOther =
    Characters { player = player, shop = shop, others = List.append others [ newOther ] }


type alias CharacterId =
    UUID


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


encodeItemDbTradeStats : ItemDbTradeStats -> Decode.Value
encodeItemDbTradeStats { times_you_sold, times_you_bought, times_others_traded } =
    Encode.object
        [ ( "times_you_sold", Encode.int times_you_sold )
        , ( "times_you_bought", Encode.int times_you_bought )
        , ( "times_others_traded", Encode.int times_others_traded )
        ]


decodeItemDbTradeStats : Decoder ItemDbTradeStats
decodeItemDbTradeStats =
    Decode.map3 ItemDbTradeStats
        (field "times_you_sold" Decode.int)
        (field "times_you_bought" Decode.int)
        (field "times_others_traded" Decode.int)


type alias ItemDb =
    Dict.Dict ItemIdStr ItemDbRecord


encodeItemId : ItemId -> Decode.Value
encodeItemId item_id =
    item_id |> UUID.toString |> Encode.string


encodeItemDbRecord : ItemDbRecord -> Decode.Value
encodeItemDbRecord { item, is_unlocked, trade_stats } =
    Encode.object
        [ ( "item_id", encodeItemId item.id )
        , ( "is_unlocked", Encode.bool is_unlocked )
        , ( "trade_stats", encodeItemDbTradeStats trade_stats )
        ]


{-| takes a default itemdb to fill the item with, and then it adds the actual data i guess
-}
decodeItemDbRecord : ItemDb -> Decoder ItemDbRecord
decodeItemDbRecord item_db =
    Decode.map3 ItemDbRecord
        (field "item_id"
            (Decode.map (lookup_item_id_default item_db) UUID.jsonDecoder)
        )
        (field "is_unlocked" Decode.bool)
        (field "trade_stats" decodeItemDbTradeStats)


encodeItemDb : ItemDb -> Decode.Value
encodeItemDb item_db =
    Encode.dict identity encodeItemDbRecord item_db


decodeItemDb : ItemDb -> Decoder ItemDb
decodeItemDb initial_item_db_ =
    Decode.map Dict.fromList <|
        Decode.keyValuePairs (decodeItemDbRecord initial_item_db_)


encodeInventoryRecord : InventoryRecord -> Decode.Value
encodeInventoryRecord { item, quantity, avg_price } =
    Encode.object
        [ ( "item_id", encodeItemId item.id )
        , ( "quantity", Encode.int <| getQuantity quantity )
        , ( "avg_price", Encode.int <| getPrice avg_price )
        ]


decodeQuantity : Decoder Quantity
decodeQuantity =
    Decode.map setQuantity
        Decode.int


decodePrice : Decoder Price
decodePrice =
    Decode.map setPrice
        Decode.int


decodeItemFromItemId : ItemDb -> Decoder Item
decodeItemFromItemId item_db =
    Decode.map (lookup_item_id_default item_db)
        UUID.jsonDecoder


decodeInventoryRecord : ItemDb -> Decoder InventoryRecord
decodeInventoryRecord item_db =
    Decode.map3 InventoryRecord
        (field "item_id" (decodeItemFromItemId item_db))
        (field "quantity" decodeQuantity)
        (field "avg_price" decodePrice)


encodeItemSentiments : ItemSentiments -> Decode.Value
encodeItemSentiments item_sentiments =
    Encode.dict String.fromInt Encode.float item_sentiments


encodeTrendTolerance : TrendTolerance -> Decode.Value
encodeTrendTolerance trend_tolerance =
    Encode.object
        [ ( "buy", encodeItemSentiments trend_tolerance.buy )
        , ( "sell", encodeItemSentiments trend_tolerance.sell )
        ]


encodeNullable : (value -> Encode.Value) -> Maybe value -> Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Encode.null


encodeItemType : ItemType -> Decode.Value
encodeItemType item_type =
    Encode.string <|
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


decodeItemType : Decoder ItemType
decodeItemType =
    Decode.string
        |> Decode.andThen
            (\item_type_str ->
                case item_type_str of
                    "Weapon" ->
                        Decode.succeed Weapon

                    "Armor" ->
                        Decode.succeed Armor

                    "Spellbook" ->
                        Decode.succeed Spellbook

                    "Furniture" ->
                        Decode.succeed Furniture

                    "Food" ->
                        Decode.succeed Food

                    _ ->
                        Decode.fail <| "Unknown item type str: " ++ item_type_str
            )


type alias CharacterPartialA =
    { held_items : InventoryRecords
    , held_gold : Int
    , char_id : CharacterId
    , name : String
    , party : TradeParty

    -- , trend_tolerance : TrendTolerance
    -- , item_types_desired : ItemSentiments
    -- , action_log : List ActionLog
    -- , hide_zero_qty_inv_rows : Bool
    -- , displayedItemType : Maybe ItemType
    -- , held_blood : Int
    }


type alias CharacterPartialB =
    -- { held_items : InventoryRecords
    -- , held_gold : Int
    -- , char_id : CharacterId
    -- , name : String
    -- , party : TradeParty
    { trend_tolerance : TrendTolerance
    , item_types_desired : ItemSentiments
    , action_log : List ActionLog
    , hide_zero_qty_inv_rows : Bool
    , displayedItemType : Maybe ItemType
    , held_blood : Int
    , held_gems : Int
    }


encodeCharacter : Character -> Value
encodeCharacter character =
    Encode.object
        [ ( "held_items", Encode.list encodeInventoryRecord character.held_items )
        , ( "held_gold", Encode.int character.held_gold )
        , ( "char_id", Encode.string <| UUID.toString character.char_id )
        , ( "name", Encode.string character.name )
        , ( "party", encodeTradeParty character.party )
        , ( "trend_tolerance", encodeTrendTolerance character.trend_tolerance )
        , ( "item_types_desired", encodeItemSentiments character.item_types_desired )
        , ( "action_log", Encode.list encodeActionLog character.action_log )
        , ( "hide_zero_qty_inv_rows", Encode.bool character.hide_zero_qty_inv_rows )
        , ( "displayedItemType", encodeNullable encodeItemType character.displayedItemType )
        , ( "held_blood", Encode.int character.held_blood )
        , ( "held_gems", Encode.int character.held_gems )
        ]


decodeCharacterA : ItemDb -> Decoder CharacterPartialA
decodeCharacterA item_db =
    Decode.map5 CharacterPartialA
        (Decode.field "held_items" (Decode.list (decodeInventoryRecord item_db)))
        (Decode.field "held_gold" Decode.int)
        (Decode.field "char_id" UUID.jsonDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "party" decodeTradeParty)



-- decodeItemSentiments : Decoder (Dict.Dict ItemTypeId Float)
-- decodeItemSentiments =


{-| if any of the first elements in the tuple are nothing, return nothing,
otherwise, return Just results
-}
combineFirst : List ( Maybe a, b ) -> Maybe (List ( a, b ))
combineFirst list =
    List.foldr
        (\( maybeA, b ) maybeAcc ->
            case ( maybeAcc, maybeA ) of
                ( Nothing, _ ) ->
                    Nothing

                ( Just _, Nothing ) ->
                    Nothing

                ( Just acc, Just a ) ->
                    Just (( a, b ) :: acc)
        )
        (Just [])
        list


decodeItemSentiments : String -> Decoder ItemSentiments
decodeItemSentiments errMsg =
    Decode.keyValuePairs Decode.float
        |> Decode.andThen
            (\pairs ->
                pairs
                    |> List.map (Tuple.mapFirst String.toInt)
                    |> combineFirst
                    |> Result.fromMaybe errMsg
                    |> DecodeExtra.fromResult
                    |> Decode.map Dict.fromList
            )


decodeTrendTolerance : Decoder TrendTolerance
decodeTrendTolerance =
    Decode.map2 TrendTolerance
        (field "buy" (decodeItemSentiments "Buy ItemSentiments has invalid item type id"))
        (field "sell" (decodeItemSentiments "Sell ItemSentiments has invalid item type id"))


decodeActionLog : Decoder ActionLog
decodeActionLog =
    Decode.map2 ActionLog
        (field "log_type" decodeActionLogType)
        (field "time" (Decode.map Time.millisToPosix Decode.int))


decodeCharacterB : Decoder CharacterPartialB
decodeCharacterB =
    Decode.map7 CharacterPartialB
        (field "trend_tolerance" decodeTrendTolerance)
        (field "item_types_desired" (decodeItemSentiments "item types desired has an invalid item type id"))
        (field "action_log" (Decode.list decodeActionLog))
        (field "hide_zero_qty_inv_rows" Decode.bool)
        (field "displayedItemType" (Decode.maybe decodeItemType))
        (field "held_blood" Decode.int)
        (field "held_gems" Decode.int)


combineCharacterPartials : CharacterPartialA -> CharacterPartialB -> Character
combineCharacterPartials charPartA charPartB =
    { -- partial A
      held_items = charPartA.held_items
    , held_gold = charPartA.held_gold
    , char_id = charPartA.char_id
    , name = charPartA.name
    , party = charPartA.party

    -- partial B
    , trend_tolerance = charPartB.trend_tolerance
    , item_types_desired = charPartB.item_types_desired
    , action_log = charPartB.action_log
    , hide_zero_qty_inv_rows = charPartB.hide_zero_qty_inv_rows
    , displayedItemType = charPartB.displayedItemType
    , held_blood = charPartB.held_blood
    , held_gems = charPartB.held_gems
    }


decodeCharacter : ItemDb -> Decoder Character
decodeCharacter item_db =
    Decode.map2 combineCharacterPartials
        (decodeCharacterA item_db)
        decodeCharacterB


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


type TabType
    = TitleScreenTabType
    | ShopTabType
    | ItemsUnlockedTabType
    | BattleTabType
    | SettingsTabType


type PlayerUpgrade
    = AutomaticGPM Int
    | AutomaticBPtoSP Int


encodePlayerUpgrade : PlayerUpgrade -> Decode.Value
encodePlayerUpgrade playerUpgrade =
    case playerUpgrade of
        AutomaticGPM level ->
            Encode.object
                [ ( "type", Encode.string "AutomaticGPM" )
                , ( "level", Encode.int level )
                ]

        AutomaticBPtoSP level ->
            Encode.object
                [ ( "type", Encode.string "AutomaticBPtoSP" )
                , ( "level", Encode.int level )
                ]


decodePlayerUpgrade : Decoder PlayerUpgrade
decodePlayerUpgrade =
    field "type" Decode.string
        |> Decode.andThen
            (\playerUpgradeStr ->
                case playerUpgradeStr of
                    "AutomaticGPM" ->
                        Decode.map AutomaticGPM (field "level" Decode.int)

                    "AutomaticBPtoSP" ->
                        Decode.map AutomaticBPtoSP (field "level" Decode.int)

                    unknownStr ->
                        Decode.fail <| "Unknown PlayerUpgrade type: " ++ unknownStr
            )


type PlayerActionLog
    = WelcomeMessageActionLog
    | TookSpecialActionInviteTrader
    | TookSpecialActionTriggerEvent SpecialEvent
    | TookSpecialActionTogglePauseAi
    | TookSpecialActionUnlockItem ItemId
    | MonsterDeliveredItemToShop ItemId


encodePlayerActionLog : PlayerActionLog -> Encode.Value
encodePlayerActionLog playerActionLog =
    case playerActionLog of
        WelcomeMessageActionLog ->
            Encode.string "WelcomeMessageActionLog"

        TookSpecialActionInviteTrader ->
            Encode.string "TookSpecialActionInviteTrader"

        TookSpecialActionTriggerEvent specialEvent ->
            Encode.object
                [ ( "type", Encode.string "TookSpecialActionTriggerEvent" )
                , ( "data", encodeSpecialEvent specialEvent )
                ]

        TookSpecialActionTogglePauseAi ->
            Encode.string "TookSpecialActionTogglePauseAi"

        TookSpecialActionUnlockItem itemId ->
            Encode.string <| "TookSpecialActionUnlockItem__" ++ UUID.toString itemId

        MonsterDeliveredItemToShop itemId ->
            Encode.string <| "TookSpecialActionUnlockItem__" ++ UUID.toString itemId


type InventorySortType
    = SortByName
    | SortByPrice
    | SortByAvgPrice
    | SortByQuantity
    | SortByItemType
    | SortByItemDesc


type alias UiOptions =
    { device : UI.Device
    , shiftIsPressed : Bool
    , hovered_trend_chart : List (CI.One TrendChartDatum CI.Dot)
    , show_main_chart : Bool
    , hoveredTooltip : UI.HoveredTooltip
    , cached_tooltip_offsets : Dict.Dict String UI.TooltipData
    , globalViewport : Maybe Browser.Dom.Viewport
    , showDebugInventoriesElement : Maybe Browser.Dom.Element
    , shouldDisplayShowDebugInventoriesOverlay : Bool
    , inventorySortType : InventorySortType
    , inventorySortDir : SortDirection
    , shop_trends_hovered : Bool
    , hovered_item_in_character : Maybe ( CharacterId, Item )
    , show_debug_inventories : Bool
    , show_charts_in_hovered_item : Bool
    }


type alias SecondsWaitedSince =
    { lastSpRefill : Int
    }


encodeSecondsWaitedSince : SecondsWaitedSince -> Encode.Value
encodeSecondsWaitedSince { lastSpRefill } =
    Encode.object [ ( "lastSpRefill", Encode.int lastSpRefill ) ]


decodeSecondsWaitedSince : Decoder SecondsWaitedSince
decodeSecondsWaitedSince =
    Decode.map SecondsWaitedSince
        (field "lastSpRefill" Decode.int)


type ProgressUnlock
    = UnlockedCharts
    | UnlockedCodex
    | UnlockedBattles
    | UnlockedDarkMode
    | UnlockedUpgrades
    | UnlockedShopTrends
    | UnlockedSpecialActions
    | UnlockedLifeQuests
    | UnlockedInventoryFilters


allProgressUnlocks : ProgressUnlocks
allProgressUnlocks =
    [ UnlockedCharts
    , UnlockedCodex
    , UnlockedBattles
    , UnlockedDarkMode
    , UnlockedUpgrades
    , UnlockedShopTrends
    , UnlockedSpecialActions
    , UnlockedLifeQuests
    , UnlockedInventoryFilters
    ]


type alias ProgressUnlocks =
    List ProgressUnlock


progressUnlockToString : ProgressUnlock -> String
progressUnlockToString progressUnlock =
    case progressUnlock of
        UnlockedCharts ->
            "Charts"

        UnlockedCodex ->
            "Codex"

        UnlockedBattles ->
            "Battles"

        UnlockedDarkMode ->
            "Dark Mode"

        UnlockedUpgrades ->
            "Upgrades"

        UnlockedShopTrends ->
            "Shop Trends"

        UnlockedSpecialActions ->
            "Special Actions"

        UnlockedLifeQuests ->
            "Life Quests"

        UnlockedInventoryFilters ->
            "Life Quests"


encodeProgressUnlock : ProgressUnlock -> Decode.Value
encodeProgressUnlock progressUnlock =
    case progressUnlock of
        UnlockedCharts ->
            Encode.string "UnlockedCharts"

        UnlockedCodex ->
            Encode.string "UnlockedCodex"

        UnlockedBattles ->
            Encode.string "UnlockedBattles"

        UnlockedDarkMode ->
            Encode.string "UnlockedDarkMode"

        UnlockedUpgrades ->
            Encode.string "UnlockedUpgrades"

        UnlockedShopTrends ->
            Encode.string "UnlockedShopTrends"

        UnlockedSpecialActions ->
            Encode.string "UnlockedSpecialActions"

        UnlockedLifeQuests ->
            Encode.string "UnlockedLifeQuests"

        UnlockedInventoryFilters ->
            Encode.string "UnlockedInventoryFilters"


decodeProgressUnlock : Decoder ProgressUnlock
decodeProgressUnlock =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "UnlockedCharts" ->
                        Decode.succeed UnlockedCharts

                    "UnlockedCodex" ->
                        Decode.succeed UnlockedCodex

                    "UnlockedBattles" ->
                        Decode.succeed UnlockedBattles

                    "UnlockedDarkMode" ->
                        Decode.succeed UnlockedDarkMode

                    "UnlockedUpgrades" ->
                        Decode.succeed UnlockedUpgrades

                    "UnlockedShopTrends" ->
                        Decode.succeed UnlockedShopTrends

                    "UnlockedSpecialActions" ->
                        Decode.succeed UnlockedSpecialActions

                    "UnlockedLifeQuests" ->
                        Decode.succeed UnlockedLifeQuests

                    _ ->
                        Decode.fail ("unregnized ProgressUnlock: " ++ str)
            )


type alias QuestTracker =
    { current : Quantity, target : Quantity }


{-| a type of quest, ie selling items or making a certain amount of money
-}
type QuestType
    = SellAnyItem QuestTracker
    | EarnGold QuestTracker


getQuestTracker : QuestType -> QuestTracker
getQuestTracker questType =
    case questType of
        EarnGold tracker ->
            tracker

        SellAnyItem tracker ->
            tracker


getQuestData : Quest -> QuestData
getQuestData quest =
    case quest of
        IncompleteQuest questData ->
            questData

        CompleteQuest questData _ ->
            questData


type alias QuestId =
    UUID


type CashedInStatus
    = QuestCashedIn
    | QuestNotCashedIn


type alias QuestData =
    { questType : QuestType
    , questId : QuestId
    }


{-| whether a quest goal has been completed or not
-}
type Quest
    = IncompleteQuest QuestData
    | CompleteQuest QuestData CashedInStatus


type alias Quests =
    { dailyQuests : List Quest, persistentQuests : List Quest }


type alias ActivePhaseData =
    { msSinceStartOfDay : Int
    , itemDbAtStart : ItemDb
    , goldAtStartOfDay : Int
    }


type alias PostPhaseData =
    { itemDbAtStart : ItemDb
    , itemDbAtEnd : ItemDb
    , goldAtStartOfDay : Int
    , goldAtEndOfDay : Int
    }


type TimePhase
    = --viewing what the day'll be (which location, how many enemy traders, any active events), maybe picking an item
      PrepPhase
    | -- ai and player upgrades tick up. itemDbAtStart is for comparing model.item_db after-hours, to show interesting facts like 'you sold X boots' or 'ais bought a lot of swords'
      ActivePhase Time.Posix ActivePhaseData
    | -- viewing day's results, shop restocks etc
      PostPhase PostPhaseData


type alias TimeOfDay =
    { -- msSinceStartOfDay : Int
      dayLengthInMs : Int
    , currentPhase : TimePhase
    }


type alias Model =
    { colorTheme : UI.ColorTheme
    , playerUpgrades : List PlayerUpgrade
    , secondsWaitedSince : SecondsWaitedSince
    , characters : Characters
    , shop_trends : ShopTrends
    , historical_shop_trends : List ShopTrends
    , historical_player_actions : List PlayerActionLog
    , item_db : ItemDb
    , ai_tick_time : Time.Posix --used to seed the ai randomness
    , globalSeed : Random.Seed --used to seed anything; will be constantly changed throughout the app
    , ai_updates_paused : Bool
    , currentTabType : TabType
    , battleModel : Battle.Model
    , browserNavKey : Maybe Nav.Key
    , uiOptions : UiOptions
    , communityFund : Int
    , progressUnlocks : ProgressUnlocks
    , quests : Quests
    , timeOfDay : TimeOfDay
    , numItemsToStartDayWith : Int
    , shouldViewGemUpgradesInPostPhase : Bool
    , titleScreenAnimationState : Animator.Timeline TitleScreenAnimationState
    , showMineGpGained : Animator.Timeline MineAnimation
    , goldGainedTimeline : Animator.Timeline GoldGainedAnimation
    , hasHadAtLeastOneBlood : Bool
    , hasHadAtLeastOneGem : Bool
    }


type GoldGainedAnimation
    = ShowGoldGainedAnimation Random.Seed
    | HideGoldAnimation Random.Seed
    | NoGoldAnimation


type MineAnimation
    = ShowMineAnimation Random.Seed
    | HideMineAnimation Random.Seed
    | NoMineAnimation


encodeModel : Model -> Decode.Value
encodeModel model =
    Encode.object
        [ ( "colorTheme", UI.encodeColorTheme model.colorTheme )
        , ( "playerUpgrades", Encode.list encodePlayerUpgrade model.playerUpgrades )
        , ( "secondsWaitedSince", encodeSecondsWaitedSince model.secondsWaitedSince )
        , ( "characters", encodeCharacters model.characters )
        , ( "shop_trends", encodeShopTrends model.shop_trends )
        , ( "historical_shop_trends", Encode.list encodeShopTrends model.historical_shop_trends )
        , ( "historical_player_actions", Encode.list encodePlayerActionLog model.historical_player_actions )
        , ( "item_db", encodeItemDb model.item_db )

        -- , ("ai_tick_time", Encode.int <| Time.posixToMillis model.ai_tick_time)
        -- , globalSeed : Random.Seed --used to seed anything; will be constantly changed throughout the app
        , ( "ai_updates_paused", Encode.bool model.ai_updates_paused )

        -- , currentTabType : TabType //NOSERIALIZE
        -- , battleModel : Battle.Model
        -- , browserNavKey : Maybe Nav.Key //NOSERIALIZE
        -- , uiOptions : UiOptions //NOSERIALIZE
        , ( "communityFund", Encode.int model.communityFund )
        , ( "hasHadAtLeastOneBlood", Encode.bool model.hasHadAtLeastOneBlood )
        , ( "hasHadAtLeastOneGem", Encode.bool model.hasHadAtLeastOneGem )
        ]


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
    , characters : Characters
    , globalSeed : Random.Seed
    , item_db : ItemDb
    , communityFund : Int
    }


type alias AiPreUpdateRecord =
    { shop_trends : ShopTrends
    , character : Character
    , shop : Shop
    , communityFund : Int
    , globalSeed : Random.Seed
    }


minutesToMillis : Int -> Int
minutesToMillis minutes =
    minutes * 60 * 1000


{-| the result of what happens during a ai\_sell/ai\_buy update.
-}
type alias AiUpdateRecord =
    { shop_trends : ShopTrends
    , character : Character
    , shop : Shop
    , traded_items : InventoryRecords
    , communityFund : Int
    , globalSeed : Random.Seed
    }


type alias TradeContext =
    { shop_trends : ShopTrends
    , from_party : Character
    , to_party : Character
    }


type TradeRecord
    = IncompleteTradeRecord TradeContext
    | CompletedTradeRecord TradeContext ItemTradeLog


wasTradeCompleted : TradeRecord -> Bool
wasTradeCompleted tradeRecord =
    case tradeRecord of
        IncompleteTradeRecord _ ->
            False

        CompletedTradeRecord _ _ ->
            True


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
            , ( False
              , { name = "Iron Boot"
                , item_type = Armor
                , raw_gold_cost = 21
                , description = "A pair of boots made of iron"
                , id = UUID.forName "iron boot" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Iron Shoulderguards"
                , item_type = Armor
                , raw_gold_cost = 17
                , description = "A pair of shoulderguards made of iron"
                , id = UUID.forName "iron shoulderguards" UUID.dnsNamespace
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
            , ( False
              , { name = "Bronze Scythe"
                , item_type = Weapon
                , raw_gold_cost = 16
                , description = "A large tool made for harvesting"
                , id = UUID.forName "bronze scythe" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Bronze calipers"
                , item_type = Weapon
                , raw_gold_cost = 10
                , description = "A small tool used for measuring. It appears to have some blood on it."
                , id = UUID.forName "bronze calipers" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Diamond-hilted Axe"
                , item_type = Weapon
                , raw_gold_cost = 100
                , description = "A ornately-decorated axe"
                , id = UUID.forName "diamond-hilted axe" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Zweihander"
                , item_type = Weapon
                , raw_gold_cost = 50
                , description = "A two-handed sword"
                , id = UUID.forName "zweihander" UUID.dnsNamespace
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
            , ( False
              , { name = "Book of Animation"
                , item_type = Spellbook
                , raw_gold_cost = 35
                , description = "Moves on its own."
                , id = UUID.forName "book of animation" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Book of Pain"
                , item_type = Spellbook
                , raw_gold_cost = 90
                , description = "Simply touching it hurts."
                , id = UUID.forName "book of pain" UUID.dnsNamespace
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
            , ( False
              , { name = "Poutine"
                , item_type = Food
                , raw_gold_cost = 25
                , description = "A foreign meal, fit for a king"
                , id = UUID.forName "poutine" UUID.dnsNamespace
                }
              )
            , ( False
              , { name = "Elvenloaf"
                , item_type = Food
                , raw_gold_cost = 95
                , description = "Strangely round load of baked flour"
                , id = UUID.forName "elvenloaf" UUID.dnsNamespace
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


initial_items_for_sale : ItemDb -> Random.Seed -> InventoryRecords
initial_items_for_sale item_db seed =
    let
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
            createCharacter (generateUuid "character 1") "Billy"

        base_character_2 =
            createCharacter (generateUuid "character 2") "Mitchell"
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


no_text_decoration : Element.Attribute msg
no_text_decoration =
    Element.htmlAttribute <| Html.Attributes.style "text-decoration" "inherit"


inherit_color : Element.Attribute msg
inherit_color =
    Element.htmlAttribute <| Html.Attributes.style "color" "initial"


itemTypeToString : ItemType -> String
itemTypeToString item_type =
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


itemTypeToString_plural : ItemType -> String
itemTypeToString_plural item_type =
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


createCharacter : UUID -> String -> Character
createCharacter char_id name =
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
    , held_blood = 0
    , held_gems = 0
    }


tabTypeToString : TabType -> String
tabTypeToString tabType =
    case tabType of
        TitleScreenTabType ->
            "title"

        ShopTabType ->
            "shop"

        ItemsUnlockedTabType ->
            "items"

        BattleTabType ->
            "battle"

        SettingsTabType ->
            "settings"


stringToTabType : String -> TabType
stringToTabType hash =
    case hash of
        "title" ->
            TitleScreenTabType

        "shop" ->
            ShopTabType

        "items" ->
            ItemsUnlockedTabType

        "battle" ->
            BattleTabType

        _ ->
            ShopTabType


init : Time.Posix -> UI.Device -> String -> Maybe Nav.Key -> ( Model, Cmd Msg )
init timeNow device hash key =
    let
        globalSeed =
            Random.initialSeed 4

        player_base_char =
            createCharacter (UUID.forName "player character" UUID.dnsNamespace) "Player"

        shop_base_char =
            createCharacter (UUID.forName "shop character" UUID.dnsNamespace) "Shop"

        item_db : ItemDb
        item_db =
            initial_item_db

        player : Player
        player =
            Player
                { player_base_char
                    | held_items = initial_owned_items item_db
                    , held_gold = 25
                    , party = PlayerParty
                    , held_blood = 100
                }

        shop : Shop
        shop =
            Shop
                { shop_base_char
                    | held_items = initial_items_for_sale item_db globalSeed
                    , held_gold = 999999999
                    , party = ShopParty
                }

        characters : Characters
        characters =
            Characters { player = player, shop = shop, others = initial_characters item_db }

        initial_currentTabType : TabType
        initial_currentTabType =
            stringToTabType hash

        spRefillUpgradeLvl =
            1

        spRefillUpgrade =
            AutomaticBPtoSP spRefillUpgradeLvl

        initialPlayerUpgrades =
            -- [ AutomaticGPM 1, spRefillUpgrade ]
            []

        battleModel : Battle.Model
        battleModel =
            Battle.init device (getInnerPlayer player) spRefillUpgradeLvl

        initUiOptions : UiOptions
        initUiOptions =
            { device = device
            , shiftIsPressed = False
            , hovered_trend_chart = []
            , show_main_chart = True
            , hoveredTooltip = UI.NoHoveredTooltip
            , cached_tooltip_offsets = Dict.empty
            , globalViewport = Nothing
            , showDebugInventoriesElement = Nothing
            , shouldDisplayShowDebugInventoriesOverlay = False
            , inventorySortType = SortByName
            , inventorySortDir = Ascending
            , show_debug_inventories = False
            , hovered_item_in_character = Nothing
            , shop_trends_hovered = False
            , show_charts_in_hovered_item = False
            }

        initModel : Model
        initModel =
            { colorTheme = BrightTheme
            , playerUpgrades = initialPlayerUpgrades
            , secondsWaitedSince = { lastSpRefill = 0 }
            , characters = characters
            , shop_trends = initial_shop_trends
            , item_db = item_db
            , historical_shop_trends = []
            , historical_player_actions = [ WelcomeMessageActionLog ]
            , ai_tick_time = timeNow
            , globalSeed = globalSeed
            , ai_updates_paused =
                --dont pause if its a game tab
                if List.member initial_currentTabType [ ShopTabType, BattleTabType, TitleScreenTabType ] then
                    False

                else
                    True
            , currentTabType = initial_currentTabType
            , battleModel = battleModel
            , browserNavKey = key
            , uiOptions = initUiOptions
            , communityFund = 0
            , progressUnlocks = []
            , quests =
                { dailyQuests =
                    []

                -- [ IncompleteQuest
                --     { questType =
                --         SellAnyItem
                --             { current = setQuantity 2
                --             , target = setQuantity 3
                --             }
                --     , questId =
                --         generateUuid "default sell quest"
                --     }
                -- , CompleteQuest
                --     { questType =
                --         EarnGold
                --             { current = setQuantity 30
                --             , target = setQuantity 30
                --             }
                --     , questId = generateUuid "default earn quest"
                --     }
                --     QuestNotCashedIn
                -- ]
                , persistentQuests = []
                }
            , timeOfDay =
                { dayLengthInMs = minutesToMillis 5

                -- , currentPhase = ActivePhase { msSinceStartOfDay = 0 }
                , currentPhase = PrepPhase
                }
            , numItemsToStartDayWith = 5
            , shouldViewGemUpgradesInPostPhase = False
            , titleScreenAnimationState = Animator.init <| HighTitle
            , showMineGpGained = Animator.init <| NoMineAnimation
            , goldGainedTimeline = Animator.init <| NoGoldAnimation
            , hasHadAtLeastOneBlood = False
            , hasHadAtLeastOneGem = False
            }
    in
    ( initModel
    , Task.perform TickSecond Time.now
    )


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .titleScreenAnimationState
            (\newState model -> { model | titleScreenAnimationState = newState })
            (\state ->
                case state of
                    HighTitle ->
                        False

                    LowTitle ->
                        True
            )
        |> Animator.watchingWith
            .showMineGpGained
            (\newState model -> { model | showMineGpGained = newState })
            (\state ->
                case state of
                    ShowMineAnimation seed ->
                        False

                    HideMineAnimation seed ->
                        True

                    NoMineAnimation ->
                        True
            )
        |> Animator.watchingWith
            .goldGainedTimeline
            (\newState model -> { model | goldGainedTimeline = newState })
            (\state ->
                case state of
                    ShowGoldGainedAnimation seed ->
                        False

                    HideGoldAnimation seed ->
                        False

                    NoGoldAnimation ->
                        True
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if not model.ai_updates_paused then
            Time.every 1000 TickSecond

          else
            Sub.none
        , Browser.Events.onKeyDown keyPressedDecoder
        , Browser.Events.onKeyUp keyReleasedDecoder
        , Sub.map GotBattleMsg <| Battle.subscriptions model.battleModel
        , Animator.toSubscription RuntimeTriggeredAnimationStep model animator
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


add_inventory_record_to_character : InventoryRecord -> Character -> Character
add_inventory_record_to_character { item, quantity, avg_price } character =
    { character
        | held_items =
            add_item_to_inventory_records
                character.held_items
                item
                quantity
                item.raw_gold_cost
    }


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
        (Maybe.withDefault 1.0
            >> (+) sentiment_delta
            >> (\sent ->
                    if sent <= 0.1 then
                        0.1

                    else
                        sent
               )
            >> Just
        )
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
                addGold new_from_party_ (setPrice <| total_cost - transaction_fee)

            new_to_party : Character
            new_to_party =
                subGold new_to_party_ (setPrice total_cost)

            new_trade_context =
                { trade_context
                    | from_party = new_from_party
                    , to_party = new_to_party
                }
        in
        case trade_record of
            CompletedTradeRecord tc tl ->
                CompletedTradeRecord new_trade_context tl

            IncompleteTradeRecord tc ->
                IncompleteTradeRecord tc

    else
        IncompleteTradeRecord orig_trade_context


{-| doesn't _add_ a character, only replaces an existing one
NOTE: silently fails without matching a character
-}
replaceCharacter : Character -> Model -> Model
replaceCharacter new_char model =
    let
        characterReplacer other_char =
            if other_char.char_id == new_char.char_id then
                new_char

            else
                other_char
    in
    { model | characters = mapCharacters characterReplacer model.characters }


updateBattleOutMsg : Battle.OutMsg -> Model -> ( Model, Cmd Msg )
updateBattleOutMsg battleOutMsg model =
    case battleOutMsg of
        Battle.OnMonsterDefeat defeatAction ->
            case defeatAction of
                Battle.NoDefeatAction ->
                    ( model, Cmd.none )

                Battle.DeliverItemToShop ->
                    let
                        ( mbNewItem, newSeed ) =
                            pick_random_unlocked_item_from_db model.item_db model.globalSeed

                        withNewItem (Shop shop) newItem =
                            replaceCharacter
                                (add_inventory_record_to_character
                                    { item = newItem
                                    , quantity = setQuantity 1
                                    , avg_price = setPrice newItem.raw_gold_cost
                                    }
                                    shop
                                )
                                { model | globalSeed = newSeed }
                                |> append_player_action_log (MonsterDeliveredItemToShop newItem.id)
                    in
                    Maybe.map
                        (withNewItem (getShop model.characters))
                        mbNewItem
                        |> Maybe.withDefault model
                        |> (\m -> ( m, Cmd.none ))

        Battle.ReturnToShop ->
            case model.browserNavKey of
                Just key ->
                    ( { model | currentTabType = ShopTabType }
                    , Nav.pushUrl
                        key
                        ("#" ++ tabTypeToString ShopTabType)
                    )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


mapPlayerIf : Bool -> (Character -> Character) -> Characters -> Characters
mapPlayerIf shouldUpdate callback ((Characters { player, shop, others }) as characters) =
    if shouldUpdate then
        mapPlayer callback characters

    else
        characters


mapPlayer : (Character -> Character) -> Characters -> Characters
mapPlayer callback (Characters { player, shop, others }) =
    Characters
        { player = Player <| callback (getInnerPlayer player)
        , shop = shop
        , others = others
        }


{-| builds a new Battle.Model with the latest data we want
-}
transferToBattleModel : Model -> Battle.Model
transferToBattleModel model =
    case getPlayer model.characters of
        Player p ->
            let
                { battleModel } =
                    model

                battlePlayer =
                    battleModel.player

                newPlayer =
                    { battlePlayer
                        | held_gold = p.held_gold
                        , held_blood = p.held_blood
                    }

                spRefillAmount =
                    List.foldl
                        (\upgrade acc ->
                            case upgrade of
                                AutomaticBPtoSP level ->
                                    level

                                _ ->
                                    acc
                        )
                        0
                        model.playerUpgrades
            in
            { battleModel | player = newPlayer, spRefillAmount = spRefillAmount }


setBattleModel : Model -> Battle.Model -> Model
setBattleModel model battleModel =
    { model | battleModel = battleModel }


withCharacters : Characters -> Model -> Model
withCharacters newCharacters model =
    { model | characters = newCharacters }


setCharacters : Model -> Characters -> Model
setCharacters model newCharacters =
    { model | characters = newCharacters }


transferFromBattleModel : Model -> Battle.Model -> Model
transferFromBattleModel model newBattleModel =
    let
        newModel =
            setBattleModel model newBattleModel

        newCharacters =
            newModel.characters
                |> --set blood and gold
                   mapPlayer
                    (\player ->
                        { player
                            | held_gold = newBattleModel.player.held_gold
                            , held_blood = newBattleModel.player.held_blood
                        }
                    )
    in
    setCharacters newModel newCharacters


updateUiOption : (UiOptions -> UiOptions) -> Model -> Model
updateUiOption updater m =
    { m | uiOptions = updater m.uiOptions }


updateUiOptions : UiOptionMsg -> Model -> ( Model, Cmd Msg )
updateUiOptions uiOptMsg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case uiOptMsg of
        MouseEnterShopItem context item ->
            ( updateUiOption (\uio -> { uio | hovered_item_in_character = Just item }) model
            , Cmd.none
            )

        MouseLeaveShopItem context item ->
            ( updateUiOption (\uio -> { uio | hovered_item_in_character = Nothing }) model
            , Cmd.none
            )

        GotTooltipMsg tooltipMsg ->
            case tooltipMsg of
                UI.StartTooltipHover tooltip_id ->
                    ( updateUiOption
                        (\uio ->
                            { uio
                                | hoveredTooltip =
                                    Dict.get tooltip_id uio.cached_tooltip_offsets
                                        |> Maybe.withDefault { offsetX = 0, offsetY = 0, hoveredTooltipId = tooltip_id }
                                        |> UI.HoveredTooltipWithoutOffset
                            }
                        )
                        model
                    , Task.attempt (GotUiOptionsMsg << GotTooltipSize) (Browser.Dom.getElement ("tooltip__" ++ tooltip_id))
                    )

                UI.EndTooltipHover tooltip_id ->
                    ( updateUiOption (\uio -> { uio | hoveredTooltip = UI.NoHoveredTooltip }) model, Cmd.none )

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

                        offsetX : Float
                        offsetX =
                            toFloat <|
                                if x < 0 then
                                    floor <| abs x + 10

                                else if x + width > viewport_width then
                                    floor <| (viewport_width - (x + width)) - 10

                                else
                                    floor <| 0

                        offsetY : Float
                        offsetY =
                            toFloat <|
                                if y < 0 then
                                    floor <| abs y + 10

                                else if y + height > viewport_height then
                                    floor <| (viewport_height - (y + height)) - 10

                                else
                                    floor <| 0
                    in
                    case model.uiOptions.hoveredTooltip of
                        UI.NoHoveredTooltip ->
                            ( model, Cmd.none )

                        UI.HoveredTooltipWithoutOffset oldTooltipData ->
                            let
                                new_tooltip_data =
                                    -- have to add the old offsets back in, because the new tooltip_size_result includes the cached size, so it needs to be accounted for
                                    { offsetX = offsetX + oldTooltipData.offsetX
                                    , offsetY = offsetY + oldTooltipData.offsetY
                                    , hoveredTooltipId = oldTooltipData.hoveredTooltipId
                                    }
                            in
                            ( updateUiOption
                                (\uio ->
                                    { uio
                                        | cached_tooltip_offsets = Dict.insert oldTooltipData.hoveredTooltipId new_tooltip_data uio.cached_tooltip_offsets
                                        , hoveredTooltip = UI.HoveredTooltipWithOffset new_tooltip_data
                                    }
                                )
                                model
                            , Cmd.none
                            )

                        UI.HoveredTooltipWithOffset oldTooltipData ->
                            let
                                new_tooltip_data =
                                    { oldTooltipData
                                        | offsetX = offsetX
                                        , offsetY = offsetY
                                    }
                            in
                            ( updateUiOption (\uio -> { uio | hoveredTooltip = UI.HoveredTooltipWithOffset new_tooltip_data }) model
                            , Cmd.none
                            )

                Err _ ->
                    noop

        StartTrendsHover ->
            ( updateUiOption (\uio -> { uio | shop_trends_hovered = True }) model, Cmd.none )

        EndTrendsHover ->
            ( updateUiOption (\uio -> { uio | shop_trends_hovered = False }) model, Cmd.none )

        ToggleShowMainChart ->
            ( updateUiOption (\uio -> { uio | show_main_chart = not uio.show_main_chart }) model, Cmd.none )

        OnTrendChartHover hovered ->
            ( updateUiOption (\uio -> { uio | hovered_trend_chart = hovered }) model, Cmd.none )

        ToggleShowDebugInventories ->
            ( updateUiOption (\uio -> { uio | show_debug_inventories = not uio.show_debug_inventories }) model, Cmd.none )

        ChangeInventorySortType inventorySortType ->
            ( updateUiOption (\uio -> { uio | inventorySortType = inventorySortType }) model, Cmd.none )

        ToggleHideNonZeroRows char_id ->
            let
                new_characters : Characters
                new_characters =
                    mapCharacters
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
                    ( replaceCharacter { character | displayedItemType = newItemType } model, Cmd.none )

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
                    ( replaceCharacter { character | displayedItemType = newItemType } model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ScrollViewport ->
            ( model, Task.perform (GotUiOptionsMsg << GotViewport) Browser.Dom.getViewport )

        GotViewport viewport ->
            ( updateUiOption (\uio -> { uio | globalViewport = Just viewport }) model
            , Task.attempt (GotUiOptionsMsg << GotShowDebugElement)
                (Browser.Dom.getElement "show_debug_inventories")
            )

        GotShowDebugElement attemptedElement ->
            ( updateUiOption
                (\uio ->
                    let
                        ( modelElement, shouldDisplayShowDebugInventoriesOverlay ) =
                            case attemptedElement of
                                Ok element ->
                                    let
                                        shouldDisplay =
                                            model.uiOptions.globalViewport
                                                |> Maybe.map
                                                    (\gvp -> isElementOnScreen gvp element)
                                                |> Maybe.withDefault False
                                    in
                                    ( Just element, shouldDisplay )

                                Err _ ->
                                    ( Nothing, False )
                    in
                    { uio
                        | showDebugInventoriesElement =
                            modelElement
                        , shouldDisplayShowDebugInventoriesOverlay =
                            shouldDisplayShowDebugInventoriesOverlay
                    }
                )
                model
            , Cmd.none
            )


onTickSecond : Model -> Time.Posix -> ( Model, Cmd Msg )
onTickSecond origModel time =
    let
        model =
            { origModel | ai_tick_time = time }

        noop =
            ( model, Cmd.none )
    in
    if not model.ai_updates_paused then
        case model.timeOfDay.currentPhase of
            ActivePhase _ _ ->
                ( model
                    |> updateActiveTimeOfDay time
                    |> update_player
                    |> update_ai_chars
                , Cmd.none
                )

            _ ->
                noop

    else
        noop


questIsCashedIn : Quest -> Bool
questIsCashedIn quest =
    case quest of
        IncompleteQuest _ ->
            False

        CompleteQuest _ cashedInStatus ->
            case cashedInStatus of
                QuestCashedIn ->
                    True

                QuestNotCashedIn ->
                    False


markAsCashedIn : Quest -> Quest
markAsCashedIn quest =
    case quest of
        IncompleteQuest _ ->
            quest

        CompleteQuest questData cashedInStatus ->
            CompleteQuest questData QuestCashedIn


onCashInQuest : Model -> QuestData -> Model
onCashInQuest ({ quests, characters } as model) { questType, questId } =
    let
        { dailyQuests, persistentQuests } =
            quests

        questMatches quest =
            let
                questData =
                    getQuestData quest

                matchesId =
                    questData.questId == questId

                isComplete =
                    isQuestTrackerComplete (getQuestTracker questData.questType)

                questCashedIn =
                    questIsCashedIn quest
            in
            matchesId && isComplete && not questCashedIn

        matchingQuests =
            List.filter questMatches
    in
    case ( matchingQuests dailyQuests, matchingQuests persistentQuests ) of
        -- neither matches
        ( [], [] ) ->
            model

        --one or more matches
        ( matchingDailyQuests, matchingPersistentQuests ) ->
            let
                (Player player) =
                    getPlayer characters

                gemsToGain =
                    List.length matchingDailyQuests + List.length matchingPersistentQuests

                newQuests =
                    { dailyQuests = List.Extra.updateIf questMatches markAsCashedIn dailyQuests
                    , persistentQuests = List.Extra.updateIf questMatches markAsCashedIn persistentQuests
                    }

                newPlayer =
                    { player | held_gems = player.held_gems + gemsToGain }
            in
            model
                |> (\m -> { m | quests = newQuests, hasHadAtLeastOneGem = True })
                |> replaceCharacter newPlayer


animateGoldGained : Animator.Timeline GoldGainedAnimation -> Random.Seed -> Animator.Timeline GoldGainedAnimation
animateGoldGained timeline seed =
    Animator.interrupt
        [ Animator.event Animator.immediately NoGoldAnimation
        , Animator.event Animator.veryQuickly (ShowGoldGainedAnimation seed)
        , Animator.wait (Animator.seconds 1)
        , Animator.event Animator.slowly (HideGoldAnimation seed)
        , Animator.event Animator.immediately NoGoldAnimation
        ]
        timeline


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        PlayerBuyItemFromShop item qty ->
            case ( getShop model.characters, getPlayer model.characters ) of
                ( Shop shop, Player player ) ->
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
                        |> replaceCharacter new_trade_context.to_party
                        |> replaceCharacter new_trade_context.from_party
                    , Cmd.none
                    )

        PlayerSellItemToShop item qty ->
            case ( getShop model.characters, getPlayer model.characters ) of
                ( Shop shop, Player player ) ->
                    let
                        trade_order =
                            { item = item, qty = qty }

                        orig_trade_context =
                            { shop_trends = model.shop_trends
                            , from_party = player
                            , to_party = shop
                            }

                        trade_record : TradeRecord
                        trade_record =
                            sell_items_from_party_to_other
                                orig_trade_context
                                trade_order

                        new_item_db =
                            updateItemDbFromTradeRecord model.item_db updateTimesYouSold trade_record
                    in
                    case trade_record of
                        CompletedTradeRecord new_trade_context item_trade_log ->
                            ( { model
                                | shop_trends = new_trade_context.shop_trends
                                , historical_shop_trends = List.append model.historical_shop_trends [ model.shop_trends ]
                                , item_db = new_item_db
                                , quests =
                                    model.quests
                                        |> playerSoldItem
                                            item_trade_log.quantity
                                        |> playerEarnedGold
                                            (setQuantity
                                                (item_trade_log.gold_cost
                                                    * getQuantity item_trade_log.quantity
                                                )
                                            )
                                , goldGainedTimeline =
                                    animateGoldGained model.goldGainedTimeline model.globalSeed
                              }
                                |> replaceCharacter new_trade_context.from_party
                                |> replaceCharacter new_trade_context.to_party
                            , Cmd.none
                            )

                        IncompleteTradeRecord _ ->
                            ( model, Cmd.none )

        TickSecond time ->
            onTickSecond model time

        ForceTickSecond ->
            ( model, Task.perform TickSecond Time.now )

        KeyPressedMsg key_event_msg ->
            case key_event_msg of
                KeyEventShift ->
                    let
                        { uiOptions } =
                            model

                        newUiOptions =
                            { uiOptions | show_charts_in_hovered_item = True, shiftIsPressed = True }
                    in
                    ( { model | uiOptions = newUiOptions }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyReleasedMsg key_event_msg ->
            case key_event_msg of
                KeyEventShift ->
                    let
                        { uiOptions } =
                            model

                        newUiOptions =
                            { uiOptions | show_charts_in_hovered_item = False, shiftIsPressed = False }
                    in
                    ( { model | uiOptions = newUiOptions }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnSpecialAction special_action price ->
            updateSpecialAction special_action price model

        ChangeTabType currentTabType ->
            ( { model | currentTabType = currentTabType }
            , case model.browserNavKey of
                Just key ->
                    Nav.pushUrl
                        key
                        ("#" ++ tabTypeToString currentTabType)

                Nothing ->
                    Cmd.none
            )

        SacrificeItem item ->
            let
                newCharacters =
                    mapPlayer
                        (\player ->
                            let
                                itemGoldCost =
                                    get_adjusted_item_cost
                                        model.shop_trends
                                        item
                                        (setQuantity 1)

                                newItems =
                                    remove_item_from_inventory_records
                                        player.held_items
                                        item
                                        (setQuantity 1)
                                        itemGoldCost
                            in
                            { player
                                | held_items = newItems
                                , held_blood = player.held_blood + itemGoldCost
                            }
                        )
                        model.characters
            in
            ( { model | characters = newCharacters, hasHadAtLeastOneBlood = True }
            , Cmd.none
            )

        ToggleColorTheme ->
            let
                newColorTheme =
                    case model.colorTheme of
                        BrightTheme ->
                            DarkTheme

                        DarkTheme ->
                            BrightTheme
            in
            ( { model | colorTheme = newColorTheme }, Cmd.none )

        GotBattleMsg battleMsg ->
            let
                ( newBattleModel, newBattleCmds, battleOutMsg ) =
                    Battle.update (transferToBattleModel model) battleMsg

                mappedCmds =
                    Cmd.map GotBattleMsg newBattleCmds

                ( newModel, newOutCmds ) =
                    newBattleModel
                        |> transferFromBattleModel model
                        |> updateBattleOutMsg battleOutMsg

                newCmds =
                    Cmd.batch (mappedCmds :: [ newOutCmds ])
            in
            ( newModel, newCmds )

        GotUiOptionsMsg uiOptMsg ->
            updateUiOptions uiOptMsg model

        ChangeCurrentPhase newPhase ->
            let
                { timeOfDay } =
                    model

                newTimeOfDay =
                    { timeOfDay | currentPhase = newPhase }
            in
            ( setTimeOfDay model newTimeOfDay, Cmd.none )

        BeginDay ->
            ( onBeginCurrentDay model, Cmd.none )

        EndDay ->
            ( onEndCurrentDay model, Cmd.none )

        CashInQuestType ({ questType, questId } as questData) ->
            if isQuestTrackerComplete (getQuestTracker questType) then
                ( onCashInQuest model questData, Cmd.none )

            else
                ( model, Cmd.none )

        ToggleViewGemUnlocksInPostPhase ->
            ( { model | shouldViewGemUpgradesInPostPhase = not model.shouldViewGemUpgradesInPostPhase }, Cmd.none )

        UnlockProgressUnlock progressUnlock gemPrice ->
            let
                (Player player) =
                    getPlayer model.characters
            in
            if player.held_gems >= getPrice gemPrice then
                ( model
                    |> (\m ->
                            { m
                                | progressUnlocks = m.progressUnlocks ++ [ progressUnlock ]
                            }
                       )
                    |> (\m ->
                            mapPlayer
                                (\p ->
                                    { p
                                        | held_gems = p.held_gems - getPrice gemPrice
                                    }
                                )
                                m.characters
                                |> setCharacters m
                       )
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RuntimeTriggeredAnimationStep newTime ->
            ( Animator.update newTime animator model, Cmd.none )

        ClickedTitleTextLabel ->
            ( { model
                | titleScreenAnimationState =
                    Animator.go
                        Animator.quickly
                        (case Animator.current model.titleScreenAnimationState of
                            HighTitle ->
                                LowTitle

                            LowTitle ->
                                HighTitle
                        )
                        model.titleScreenAnimationState
              }
            , Cmd.none
            )

        ClickedTitlePlayLabel ->
            ( { model | currentTabType = ShopTabType }, Cmd.none )



--- END OF UPDATE


setTimeOfDay : Model -> TimeOfDay -> Model
setTimeOfDay model newTimeOfDay =
    { model | timeOfDay = newTimeOfDay }


mapIncompleteQuestType : (QuestType -> QuestId -> Quest) -> Quest -> Quest
mapIncompleteQuestType mapper quest =
    case quest of
        IncompleteQuest { questType, questId } ->
            mapper questType questId

        _ ->
            quest


{-| triggered when you click 'Begin Day' from the prep screen. All this does
is change the current phase to ActivePhase
-}
onBeginCurrentDay : Model -> Model
onBeginCurrentDay ({ timeOfDay, item_db, globalSeed, characters, ai_tick_time } as model) =
    let
        newTimeOfDay =
            { timeOfDay
                | currentPhase =
                    ActivePhase
                        ai_tick_time
                        { goldAtStartOfDay =
                            let
                                (Player player) =
                                    getPlayer characters
                            in
                            player.held_gold
                        , msSinceStartOfDay = 0
                        , itemDbAtStart = item_db
                        }
            }
    in
    { model
        | timeOfDay = newTimeOfDay
    }


{-| triggered when you click 'Go to sleep' from the post screen.
-}
onEndCurrentDay : Model -> Model
onEndCurrentDay ({ timeOfDay, item_db, globalSeed, characters, ai_tick_time } as model) =
    onPrepNewDay model


{-| called at the very start of a new day, where it preps all the new items and
everything so that it can be displayed. stuff like which items will be for
sale, who you'll be fighting against, any active modifiers (todo) etc
-}
onPrepNewDay : Model -> Model
onPrepNewDay ({ timeOfDay, item_db, globalSeed, characters, ai_tick_time, quests } as model) =
    let
        (Shop shop) =
            getShop characters

        newShop : Character
        newShop =
            List.foldl
                (\item shop_ -> addHeldItem item shop_)
                { shop | held_items = [] }
                newShopItems

        ( globalSeedForUuid, newShopItems ) =
            List.foldl
                (\_ ( seed, items ) ->
                    let
                        ( mbItem, newSeed ) =
                            pick_random_unlocked_item_from_db item_db seed

                        newItems : List Item
                        newItems =
                            case mbItem of
                                Just newItem ->
                                    newItem :: items

                                Nothing ->
                                    items
                    in
                    ( newSeed, newItems )
                )
                ( globalSeed, [] )
                --List.range is inclusive (List.range 0 1 == [0, 1])
                (List.range 0 (model.numItemsToStartDayWith - 1))

        ( questId, newGlobalSeed ) =
            Random.step UUID.generator globalSeedForUuid

        newQuests : Quests
        newQuests =
            { quests
                | dailyQuests =
                    [ IncompleteQuest
                        { questType =
                            EarnGold
                                { current = setQuantity 0
                                , target = setQuantity 50
                                }
                        , questId =
                            questId
                        }
                    ]
            }
    in
    { model
        | characters = setShop (Shop newShop) model.characters
        , globalSeed = newGlobalSeed
        , quests = newQuests
        , timeOfDay = { timeOfDay | currentPhase = PrepPhase }
    }


updateActiveTimeOfDay : Time.Posix -> Model -> Model
updateActiveTimeOfDay newTime ({ ai_tick_time, timeOfDay } as model) =
    case timeOfDay.currentPhase of
        ActivePhase timeDayStarted { msSinceStartOfDay, itemDbAtStart, goldAtStartOfDay } ->
            let
                msDiff =
                    Time.posixToMillis newTime - Time.posixToMillis timeDayStarted

                newMsSinceStartOfDay =
                    msSinceStartOfDay + msDiff

                isWithinCurrentDay =
                    newMsSinceStartOfDay < timeOfDay.dayLengthInMs

                (Player player) =
                    getPlayer model.characters

                newPhase =
                    if isWithinCurrentDay then
                        ActivePhase
                            model.ai_tick_time
                            { goldAtStartOfDay = goldAtStartOfDay
                            , msSinceStartOfDay = newMsSinceStartOfDay
                            , itemDbAtStart = itemDbAtStart
                            }

                    else
                        PostPhase
                            { goldAtStartOfDay = goldAtStartOfDay
                            , goldAtEndOfDay = player.held_gold
                            , itemDbAtStart = itemDbAtStart
                            , itemDbAtEnd = model.item_db
                            }
            in
            { model
                | timeOfDay =
                    { timeOfDay
                        | currentPhase =
                            newPhase
                    }
            }

        _ ->
            --NOOP
            model


isQuestTrackerComplete : QuestTracker -> Bool
isQuestTrackerComplete questTracker =
    questTracker.current == questTracker.target


{-| NOTE: assumes the quest isn't already complete
-}
onSellAnyItem : QuestTracker -> QuestId -> Quantity -> Quest
onSellAnyItem { current, target } questId soldQty =
    let
        newTracker =
            { current = minQuantity (addQuantity current soldQty) target
            , target = target
            }

        newQuestData : QuestData
        newQuestData =
            { questType = SellAnyItem newTracker, questId = questId }
    in
    if isQuestTrackerComplete newTracker then
        CompleteQuest newQuestData QuestNotCashedIn

    else
        IncompleteQuest newQuestData


{-| assumes the quest is Incomplete so we can set a fresh QuestNotCashedIn
-}
onEarnGold : QuestTracker -> QuestId -> Quantity -> Quest
onEarnGold { current, target } questId soldQty =
    let
        newTracker =
            { current = minQuantity (addQuantity current soldQty) target
            , target = target
            }

        newQuestData : QuestData
        newQuestData =
            { questType = EarnGold newTracker
            , questId = questId
            }
    in
    if isQuestTrackerComplete newTracker then
        CompleteQuest newQuestData QuestNotCashedIn

    else
        IncompleteQuest newQuestData


playerSoldItem : Quantity -> Quests -> Quests
playerSoldItem soldQty { dailyQuests, persistentQuests } =
    let
        questUpdater =
            mapIncompleteQuestType
                (\questType questId ->
                    case questType of
                        SellAnyItem questTracker ->
                            onSellAnyItem questTracker questId soldQty

                        EarnGold questTracker ->
                            onEarnGold questTracker questId soldQty
                )
    in
    { dailyQuests = List.map questUpdater dailyQuests
    , persistentQuests = List.map questUpdater persistentQuests
    }


playerEarnedGold : Quantity -> Quests -> Quests
playerEarnedGold earnedGold { dailyQuests, persistentQuests } =
    let
        questUpdater =
            mapIncompleteQuestType
                (\questType questId ->
                    case questType of
                        EarnGold questTracker ->
                            onEarnGold questTracker questId earnedGold

                        _ ->
                            -- we know we can return an IncompleteQuest because this is a function that only deals with IncompleteQuests
                            IncompleteQuest { questType = questType, questId = questId }
                )
    in
    { dailyQuests = List.map questUpdater dailyQuests
    , persistentQuests = List.map questUpdater persistentQuests
    }


generateUuid : String -> UUID.UUID
generateUuid str =
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


handleInviteTrader : Model -> Model
handleInviteTrader model =
    let
        { characters, item_db, globalSeed } =
            model

        name =
            "Character " ++ (String.fromInt <| charactersLength model.characters + 1)

        invited_character =
            createCharacter (generateUuid name) name

        ( num_items, _ ) =
            Random.step (Random.int 1 5) globalSeed

        ( new_globalSeed, held_maybe_item_frames ) =
            List.foldl
                (pick_item item_db)
                ( globalSeed, [] )
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
            addOther
                characters
                { invited_character
                    | held_gold = 50
                    , held_items = held_items
                }
        , globalSeed = new_globalSeed
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


handleSpecialEvent : Model -> SpecialEvent -> Model
handleSpecialEvent model spec_event =
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
                    choose_item_type model.globalSeed
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
                    choose_item_type model.globalSeed
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
    { model | globalSeed = new_seed }


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


specialActionIncreaseBpToSp : Model -> Model
specialActionIncreaseBpToSp model =
    let
        (Player player) =
            getPlayer model.characters
    in
    let
        automaticBPtoSPLevel =
            model.playerUpgrades
                |> List.filterMap
                    (\upgrade ->
                        case upgrade of
                            AutomaticBPtoSP level ->
                                Just level

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.withDefault 1

        upgradeCost : Price
        upgradeCost =
            scale_increase_bp_to_sp_cost automaticBPtoSPLevel

        doUpgrade =
            \upgrade ->
                case upgrade of
                    AutomaticBPtoSP level ->
                        AutomaticBPtoSP (level + 1)

                    _ ->
                        upgrade
    in
    if hasEnoughGold player upgradeCost then
        model
            |> replaceCharacter (subGold player upgradeCost)
            |> (\m ->
                    { m | playerUpgrades = List.map doUpgrade m.playerUpgrades }
               )

    else
        model


specialActionIncreaseIncome : Model -> Model
specialActionIncreaseIncome model =
    let
        (Player player) =
            getPlayer model.characters
    in
    let
        automaticGpmLevel =
            model.playerUpgrades
                |> List.filterMap
                    (\upgrade ->
                        case upgrade of
                            AutomaticGPM level ->
                                Just level

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.withDefault 1

        upgradeCost : Price
        upgradeCost =
            scale_increase_income_cost automaticGpmLevel
    in
    if hasEnoughGold player upgradeCost then
        model
            |> replaceCharacter
                (subGold player upgradeCost)
            |> (\m ->
                    { m
                        | playerUpgrades =
                            List.map
                                (\upgrade ->
                                    case upgrade of
                                        AutomaticGPM level ->
                                            AutomaticGPM (level + 1)

                                        _ ->
                                            upgrade
                                )
                                m.playerUpgrades
                    }
               )

    else
        model


specialActionUnlockItem : Model -> Model
specialActionUnlockItem model =
    let
        { item_db, globalSeed } =
            model

        ( mb_item_to_unlock, new_seed ) =
            pick_random_locked_item_from_db item_db <| globalSeed
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
        , globalSeed = new_seed
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


addGold : Character -> Price -> Character
addGold character price =
    { character | held_gold = character.held_gold + getPrice price }


subGold : Character -> Price -> Character
subGold character price =
    { character | held_gold = character.held_gold - getPrice price }


communityFundCost : Price
communityFundCost =
    setPrice 25


{-| adds money to the pool to allow AIs to get new items
-}
specialActionCommunityFund : Model -> Model
specialActionCommunityFund model =
    let
        price =
            communityFundCost
    in
    getPlayer model.characters
        |> (\(Player player) ->
                if hasEnoughGold player price then
                    model
                        |> replaceCharacter (subGold player price)
                        |> (\m ->
                                { m
                                    | communityFund = m.communityFund + getPrice price
                                }
                           )

                else
                    model
           )


mineSuccessAnimation : Animator.Timeline MineAnimation -> Random.Seed -> Animator.Timeline MineAnimation
mineSuccessAnimation timeline seed =
    Animator.interrupt
        [ Animator.event Animator.immediately NoMineAnimation
        , Animator.event Animator.veryQuickly (ShowMineAnimation seed)
        , Animator.wait (Animator.seconds 1)
        , Animator.event Animator.slowly (HideMineAnimation seed)
        , Animator.event Animator.immediately NoMineAnimation
        ]
        timeline


updateMine : Model -> ( Model, Cmd Msg )
updateMine ({ globalSeed } as model) =
    let
        ( shouldEarnGp, newSeed ) =
            Random.step
                (Random.int 0 100
                    |> Random.map (\rnd -> rnd <= 15)
                )
                globalSeed

        gpEarned =
            1

        newCharacters : Characters
        newCharacters =
            mapPlayerIf
                shouldEarnGp
                (\p ->
                    { p | held_gold = p.held_gold + gpEarned }
                )
                model.characters

        mineCmd =
            if not shouldEarnGp then
                playMineSound

            else
                playMineSuccessSound
    in
    ( model
        |> (\m ->
                if shouldEarnGp then
                    { m
                        | showMineGpGained =
                            mineSuccessAnimation m.showMineGpGained m.globalSeed
                        , goldGainedTimeline =
                            animateGoldGained m.goldGainedTimeline m.globalSeed
                    }

                else
                    m
           )
        |> setGlobalSeed newSeed
        |> withCharacters newCharacters
    , mineCmd
    )


decodeAndPlaySound : String -> Cmd msg
decodeAndPlaySound soundConfigStr =
    soundConfigStr
        |> Sfxr.decodeAndPlaySoundJson
        |> Result.withDefault Cmd.none


playMineSound : Cmd msg
playMineSound =
    decodeAndPlaySound Sfxr.mineHitConfig


playMineSuccessSound : Cmd msg
playMineSuccessSound =
    decodeAndPlaySound Sfxr.mineSuccessConfig


updateSpecialAction : SpecialAction -> Price -> Model -> ( Model, Cmd Msg )
updateSpecialAction special_action price origModel =
    case getPlayer origModel.characters of
        Player player ->
            if hasEnoughGold player price then
                origModel
                    |> replaceCharacter (subGold player price)
                    |> (\model ->
                            case special_action of
                                InviteTrader ->
                                    ( model
                                        |> handleInviteTrader
                                        |> handleInviteTrader
                                        |> handleInviteTrader
                                        |> handleInviteTrader
                                        |> handleInviteTrader
                                        |> handleInviteTrader
                                        |> handleInviteTrader
                                        |> handleInviteTrader
                                    , Cmd.none
                                    )

                                Mine ->
                                    updateMine model

                                TriggerEvent event ->
                                    ( handleSpecialEvent model event, Cmd.none )

                                TogglePauseAi ->
                                    ( { model | ai_updates_paused = not model.ai_updates_paused } |> append_player_action_log TookSpecialActionTogglePauseAi, Cmd.none )

                                UnlockItem ->
                                    ( specialActionUnlockItem model, Cmd.none )

                                IncreaseIncome ->
                                    ( specialActionIncreaseIncome model, Cmd.none )

                                IncreaseBPtoSP ->
                                    ( specialActionIncreaseBpToSp model, Cmd.none )

                                CommunityFund ->
                                    ( specialActionCommunityFund model, Cmd.none )
                       )

            else
                ( origModel, Cmd.none )


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


bloodCostForRefillSp =
    3


addGolemSpFromBlood : Character -> Int -> Battle.Model -> ( Character, Battle.Model )
addGolemSpFromBlood ({ held_blood } as player) level battleModel =
    let
        missingStamina =
            Battle.monsterMap (\m -> Battle.getStatMissingVal m.statStamina) battleModel.golem

        spToRefill : Int
        spToRefill =
            min (level * 1) missingStamina

        newHeldBlood =
            held_blood - (bloodCostForRefillSp * spToRefill)
    in
    -- if player has enough gold, subtract it, and add 1*level SP to golem
    if
        (held_blood >= (bloodCostForRefillSp * spToRefill))
            && Battle.doesGolemNeedStamina battleModel
    then
        ( { player | held_blood = newHeldBlood }
        , Battle.increaseGolemStamina battleModel spToRefill
        )

    else
        ( player, battleModel )


setSecondsWaited : Model -> SecondsWaitedSince -> Model
setSecondsWaited model newSecondsWaitedSince =
    { model | secondsWaitedSince = newSecondsWaitedSince }


applyUpgrade : PlayerUpgrade -> ( Character, Model ) -> ( Character, Model )
applyUpgrade upgrade ( player, { secondsWaitedSince } as model ) =
    case upgrade of
        AutomaticGPM to_add ->
            let
                newPlayer =
                    add_player_gpm player to_add
            in
            ( newPlayer, replaceCharacter newPlayer model )

        AutomaticBPtoSP level ->
            if model.secondsWaitedSince.lastSpRefill >= Battle.secondsRequiredForSpRefill then
                let
                    ( newPlayer, newBattleModel ) =
                        addGolemSpFromBlood player level model.battleModel

                    newSecondsWaitedSince =
                        { secondsWaitedSince | lastSpRefill = 0 }
                in
                ( newPlayer
                , replaceCharacter newPlayer model
                    |> (\m -> { m | battleModel = newBattleModel })
                    |> (\m -> setSecondsWaited m newSecondsWaitedSince)
                )

            else
                let
                    newSecondsWaitedSince =
                        { secondsWaitedSince | lastSpRefill = secondsWaitedSince.lastSpRefill + 1 }
                in
                ( player, setSecondsWaited model newSecondsWaitedSince )


applyUpgrades : Character -> Model -> Model
applyUpgrades player model =
    let
        ( new_player, new_model ) =
            List.foldl applyUpgrade ( player, model ) model.playerUpgrades
    in
    new_model


update_player : Model -> Model
update_player model =
    case getPlayer model.characters of
        Player player ->
            applyUpgrades player model


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


{-| items the character can afford and desires at least a little
-}
get_wanted_items : Character -> Shop -> ShopTrends -> InventoryRecords
get_wanted_items character shop shop_trends =
    List.filter
        (\inventory_record ->
            nonzero_qty inventory_record
                && check_can_afford_one character shop_trends inventory_record.item
                && check_nonzero_desire character inventory_record.item
        )
        (getShopCharacter shop).held_items


ai_buy_item_from_shop : Time.Posix -> ItemDb -> AiPreUpdateRecord -> AiUpdateRecord
ai_buy_item_from_shop ai_tick_time item_db { shop_trends, character, shop, communityFund, globalSeed } =
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
                (group_shuffle_items globalSeed least_trendy_items)

        trade_record : TradeRecord
        trade_record =
            case maybe_item_to_buy of
                Nothing ->
                    IncompleteTradeRecord
                        { shop_trends = shop_trends
                        , from_party = getShopCharacter shop
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
                        , from_party = getShopCharacter shop
                        , to_party = character
                        }
                        { item = item, qty = qty_to_buy }
    in
    case trade_record of
        IncompleteTradeRecord trade_context_ ->
            { shop_trends = trade_context_.shop_trends
            , character = trade_context_.to_party
            , shop = Shop trade_context_.from_party
            , traded_items = []
            , communityFund = communityFund
            , globalSeed = globalSeed
            }

        CompletedTradeRecord trade_context_ log ->
            { shop_trends = trade_context_.shop_trends
            , character =
                append_to_character_action_log
                    trade_context_.to_party
                    { log_type = Traded log, time = ai_tick_time }
            , shop = Shop trade_context_.from_party
            , traded_items =
                case lookup_item_id item_db log.item_id of
                    Just item_trade_log ->
                        [ { item = item_trade_log.item, quantity = log.quantity, avg_price = Price log.gold_cost } ]

                    Nothing ->
                        -- []
                        Debug.todo ""
            , communityFund = communityFund
            , globalSeed = globalSeed
            }



-- tradeRecordToAiUpdate


ai_sell_item_to_shop : Time.Posix -> ItemDb -> AiPreUpdateRecord -> AiUpdateRecord
ai_sell_item_to_shop ai_tick_time item_db { shop_trends, character, shop, communityFund, globalSeed } =
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
                        , to_party = getShopCharacter shop
                        }

                Just { item } ->
                    sell_items_from_party_to_other
                        { shop_trends = shop_trends
                        , from_party = character
                        , to_party = getShopCharacter shop
                        }
                        { item = item, qty = qty_to_sell }
    in
    case trade_record of
        IncompleteTradeRecord trade_context_ ->
            { shop_trends = trade_context_.shop_trends
            , character = trade_context_.from_party
            , shop = Shop trade_context_.to_party
            , traded_items = []
            , communityFund = communityFund
            , globalSeed = globalSeed
            }

        CompletedTradeRecord trade_context_ log ->
            { shop_trends = trade_context_.shop_trends
            , character =
                append_to_character_action_log
                    trade_context_.from_party
                    { log_type = Traded log, time = ai_tick_time }
            , shop = Shop trade_context_.to_party
            , traded_items =
                case lookup_item_id item_db log.item_id of
                    Just item_trade_log ->
                        [ { item = item_trade_log.item, quantity = log.quantity, avg_price = Price log.gold_cost } ]

                    Nothing ->
                        Debug.todo "" []
            , communityFund = communityFund
            , globalSeed = globalSeed
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


{-| adds a single item for its raw gold cost
-}
addHeldItem : Item -> Character -> Character
addHeldItem item ({ held_items } as character) =
    { character
        | held_items =
            add_item_to_inventory_records
                held_items
                item
                (setQuantity 1)
                item.raw_gold_cost
    }


convertPreUpdateRecordToPostUpdate : AiPreUpdateRecord -> AiUpdateRecord
convertPreUpdateRecordToPostUpdate { shop_trends, character, shop, communityFund, globalSeed } =
    { shop_trends = shop_trends
    , character = character
    , shop = shop
    , communityFund = communityFund
    , traded_items = []
    , globalSeed = globalSeed
    }


ai_fetch_item : Time.Posix -> ItemDb -> AiPreUpdateRecord -> AiUpdateRecord
ai_fetch_item ai_tick_time item_db ({ shop_trends, character, shop, communityFund, globalSeed } as preUpdateRecord) =
    let
        ( mbNewItem, newSeed ) =
            pick_random_unlocked_item_from_db item_db globalSeed
    in
    case mbNewItem of
        Just newItem ->
            if newItem.raw_gold_cost <= communityFund then
                { shop_trends = shop_trends
                , character =
                    character
                        |> addHeldItem newItem
                        |> (\c ->
                                append_to_character_action_log c
                                    { log_type = FetchedItem newItem.id
                                    , time = ai_tick_time
                                    }
                           )
                , shop = shop
                , traded_items = []
                , communityFund = communityFund - newItem.raw_gold_cost
                , globalSeed = newSeed
                }

            else
                preUpdateRecord
                    |> convertPreUpdateRecordToPostUpdate
                    |> (\ai_update_record ->
                            { ai_update_record
                                | character =
                                    append_to_character_action_log character
                                        { log_type = FetchedItemButFundNotBigEnough newItem.id
                                        , time = ai_tick_time
                                        }
                                , globalSeed = newSeed
                            }
                       )

        Nothing ->
            convertPreUpdateRecordToPostUpdate preUpdateRecord


pickAiActionChoice : Random.Seed -> ( AiActionChoice, Random.Seed )
pickAiActionChoice seed =
    (List.repeat 10 WantsToSell
        ++ List.repeat 10 WantsToBuy
        ++ List.repeat 2 WantsToFetchItem
        ++ List.repeat 5 NoActionChoice
    )
        |> Random.List.choose
        |> (\choices -> Random.step choices seed)
        |> Tuple.mapFirst
            (Tuple.first >> Maybe.withDefault NoActionChoice)


update_ai : Time.Posix -> CharacterId -> AiUpdateData -> AiUpdateData
update_ai ai_tick_time char_id ({ shop_trends, historical_shop_trends, characters, globalSeed, item_db, communityFund } as original_ai_update_data) =
    let
        --TODO: make sure character isn't shop
        maybe_character =
            getCharacter characters char_id

        shop =
            getShop characters
    in
    case maybe_character of
        Just character ->
            let
                -- chosen_action, new_seed : (AiActionChoice, Random.Seed)
                ( chosen_action, new_seed ) =
                    pickAiActionChoice globalSeed

                preUpdateRecord : AiPreUpdateRecord
                preUpdateRecord =
                    { shop_trends = shop_trends
                    , character = character
                    , shop = shop
                    , communityFund = communityFund
                    , globalSeed = new_seed
                    }

                ai_update_record : AiUpdateRecord
                ai_update_record =
                    case chosen_action of
                        WantsToSell ->
                            ai_sell_item_to_shop
                                ai_tick_time
                                item_db
                                preUpdateRecord

                        WantsToBuy ->
                            ai_buy_item_from_shop
                                ai_tick_time
                                item_db
                                preUpdateRecord

                        WantsToFetchItem ->
                            ai_fetch_item
                                ai_tick_time
                                item_db
                                preUpdateRecord

                        NoActionChoice ->
                            { shop_trends = preUpdateRecord.shop_trends
                            , character =
                                append_to_character_action_log preUpdateRecord.character
                                    { log_type = DidNothing, time = ai_tick_time }
                            , shop = preUpdateRecord.shop
                            , traded_items = []
                            , communityFund = preUpdateRecord.communityFund
                            , globalSeed = preUpdateRecord.globalSeed
                            }

                new_characters : Characters
                new_characters =
                    characters
                        |> mapCharacters
                            (\c ->
                                if c.char_id == character.char_id then
                                    ai_update_record.character

                                else
                                    c
                            )
                        |> setShop ai_update_record.shop

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
            , globalSeed = new_seed
            , item_db = new_item_db
            , communityFund = ai_update_record.communityFund
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

        { ai_tick_time, item_db, communityFund, globalSeed } =
            model

        first_ai_update_data : AiUpdateData
        first_ai_update_data =
            { shop_trends = old_shop_trends
            , historical_shop_trends = old_historical_shop_trends
            , characters = old_characters
            , globalSeed = globalSeed
            , item_db = item_db
            , communityFund = communityFund
            }

        new_ai_data : AiUpdateData
        new_ai_data =
            old_characters
                |> getOthers
                |> List.map .char_id
                |> List.foldl
                    (update_ai ai_tick_time)
                    first_ai_update_data
    in
    { model
        | shop_trends = new_ai_data.shop_trends
        , historical_shop_trends = new_ai_data.historical_shop_trends
        , characters = new_ai_data.characters
        , item_db = new_ai_data.item_db
        , globalSeed = new_ai_data.globalSeed
    }


get_trend_color : Float -> Color
get_trend_color trend =
    if trend > 1.65 then
        UI.color_pastel_red_7

    else if trend > 1.55 then
        UI.color_pastel_red_6

    else if trend > 1.45 then
        UI.color_pastel_red_5

    else if trend > 1.35 then
        UI.color_pastel_red_4

    else if trend > 1.25 then
        UI.color_pastel_red_3

    else if trend > 1.15 then
        UI.color_pastel_red_2

    else if trend > 1.0 then
        UI.color_pastel_red_1

    else if trend < 0.45 then
        UI.color_pastel_green_7

    else if trend < 0.55 then
        UI.color_pastel_green_6

    else if trend < 0.65 then
        UI.color_pastel_green_5

    else if trend < 0.75 then
        UI.color_pastel_green_4

    else if trend < 0.85 then
        UI.color_pastel_green_3

    else if trend < 0.95 then
        UI.color_pastel_green_2

    else if trend < 1.0 then
        UI.color_pastel_green_1

    else
        rgb 0 0 0


renderItemTypeWithTrend : ShopTrends -> ItemType -> Element.Element Msg
renderItemTypeWithTrend shop_trends item_type =
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
            itemTypeToString item_type
        , text " - "
        , el ([ Font.color trend_color ] ++ trend_shadow) <| text pretty_trend
        ]


renderItemTypeWithoutTrend : ItemType -> Element.Element Msg
renderItemTypeWithoutTrend item_type =
    Element.paragraph [ Font.alignLeft, width fill ] <|
        [ text <|
            itemTypeToString item_type
        ]


shop_buy_button : ColorTheme -> Int -> Int -> InventoryRecord -> Element Msg
shop_buy_button colorTheme gold_cost gold_in_pocket { item, quantity, avg_price } =
    let
        can_afford =
            gold_in_pocket >= gold_cost

        button_type =
            if can_afford then
                UI.Primary

            else
                UI.Secondary
    in
    UI.button <|
        UI.TextParams
            { buttonType = button_type
            , colorTheme = colorTheme
            , customAttrs =
                [ getQuantity quantity < 1 |> Element.transparent
                , width (fill |> Element.minimum 120)
                ]
            , onPressMsg =
                PlayerBuyItemFromShop item (Quantity 1)
            , textLabel =
                if can_afford then
                    "BUY"

                else
                    "Need GP"
            }


shop_sell_button : ColorTheme -> Bool -> InventoryRecord -> Element Msg
shop_sell_button colorTheme has_items_to_sell_ { item } =
    let
        button_type =
            if has_items_to_sell_ then
                UI.Primary

            else
                UI.Secondary

        buttonText =
            if has_items_to_sell_ then
                "SELL"

            else
                "Need GP"
    in
    UI.button <|
        UI.TextParams
            { buttonType = button_type
            , customAttrs =
                [ Element.transparent <| not has_items_to_sell_
                , width (fill |> Element.minimum 120)
                ]
            , onPressMsg =
                PlayerSellItemToShop item (Quantity 1)
            , textLabel = buttonText
            , colorTheme = colorTheme
            }


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


render_single_trade_log_entry : UI.ColorTheme -> ItemDb -> Characters -> ItemTradeLog -> Element msg
render_single_trade_log_entry colorTheme item_db ((Characters { player, shop, others }) as characters) trade_log =
    let
        { from_party, to_party, item_id, quantity, gold_cost } =
            trade_log

        maybe_item =
            lookup_item_id item_db item_id

        qty_str =
            String.fromInt (getQuantity quantity) |> (++) "x"

        rendered_cost : Element msg
        rendered_cost =
            UI.renderGp colorTheme gold_cost

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
                        ++ trade_party_to_str characters to_party
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
                        ++ trade_party_to_str characters from_party
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
                        ++ trade_party_to_str characters from_party
                        ++ " to "
                        ++ trade_party_to_str characters to_party
                        ++ " ("
                        ++ qty_str
                        ++ ") "
                        ++ "was traded for "
                , rendered_cost
                ]


trends_display : UI.ColorTheme -> Bool -> ItemDb -> ShopTrends -> Characters -> Bool -> Element.Element Msg
trends_display colorTheme shiftIsPressed item_db shop_trends ((Characters { player, shop, others }) as characters) is_expanded =
    let
        render_single_popularity : ( Int, Float ) -> Element.Element msg
        render_single_popularity ( type_id, popularity ) =
            let
                pretty_type : String
                pretty_type =
                    case id_to_item_type type_id of
                        Just prettied ->
                            itemTypeToString_plural prettied

                        Nothing ->
                            "Unknown Type (" ++ String.fromInt type_id ++ ")"
            in
            paragraph []
                [ text <| pretty_type
                , text ": "
                , UI.monospace [] <|
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
                    ++ (String.fromInt <| charactersLength characters)
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
                    , UI.defaultBackgroundColor colorTheme
                    , Border.color <| rgb 0.35 0.35 0.35
                    , Border.rounded 3
                    , Border.width 2
                    , padding 10
                    , spacing 5
                    , Element.moveDown 20
                    ]
                <|
                    [ row
                        [ Font.color <|
                            case colorTheme of
                                BrightTheme ->
                                    UI.color_grey

                                DarkTheme ->
                                    UI.convertColor Color.lightGrey
                        , Font.size 12
                        , width fill
                        , Element.spaceEvenly
                        ]
                        [ text "Latest first"
                        , if shiftIsPressed then
                            Element.none

                          else
                            text "Hold shift for more"
                        ]
                    ]
                        ++ (List.map
                                (render_single_trade_log_entry colorTheme item_db characters)
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
                    [ Events.onMouseEnter <| GotUiOptionsMsg StartTrendsHover
                    , Events.onMouseLeave <| GotUiOptionsMsg EndTrendsHover
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
        [ el [ UI.font_scaled 2, UI.border_bottom 2 ] <| text "Shop Trends"
        , rendered_popularity
        ]


divider : List (Element msg)
divider =
    [ Element.el [ width fill, paddingXY 50 5 ] <|
        Element.el
            [ width fill
            , UI.border_bottom 1
            , Border.color UI.color_very_light_grey
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


action_log_to_str : UI.ColorTheme -> ItemDb -> ActionLog -> String
action_log_to_str colorTheme item_db action_log =
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
            "Fetched an item: " ++ (lookup_item_id_default item_db itemId).name

        FetchedItemButFundNotBigEnough itemId ->
            "Tried to fetch an item, but the Community Fund didn't contain enough gold. Needed" ++ UI.renderGpString (lookup_item_id_default item_db itemId).raw_gold_cost

        DidNothing ->
            "Did nothing"


hasProgressUnlock : ProgressUnlock -> Model -> Bool
hasProgressUnlock progressUnlock model =
    containsProgressUnlock progressUnlock model.progressUnlocks


containsProgressUnlock : ProgressUnlock -> ProgressUnlocks -> Bool
containsProgressUnlock progressUnlock progressUnlocks =
    List.member progressUnlock progressUnlocks



-- True


getProgressUnlockGemPrice : ProgressUnlock -> Price
getProgressUnlockGemPrice progressUnlock =
    let
        cosmeticUnlocks =
            [ UnlockedDarkMode, UnlockedCodex, UnlockedCharts, UnlockedShopTrends, UnlockedInventoryFilters ]

        mechanicalUnlocks =
            [ UnlockedLifeQuests, UnlockedSpecialActions ]

        specialUnlocks =
            [ UnlockedBattles ]

        containsUnlock unlocks =
            List.member progressUnlock unlocks
    in
    if containsUnlock specialUnlocks then
        setPrice 3

    else if containsUnlock mechanicalUnlocks then
        setPrice 2

    else if containsUnlock cosmeticUnlocks then
        setPrice 1

    else
        setPrice 1


buildCompare : (a -> comparable) -> (a -> a -> Order)
buildCompare getter =
    \left right ->
        compare (getter left) (getter right)


renderDesires : ItemSentiments -> List (Element Msg)
renderDesires item_types_desired =
    Dict.toList item_types_desired
        |> List.filter (Tuple.second >> (\trd -> trd > 0.0))
        |> List.map
            (\( it_id, trd ) ->
                text <|
                    "Desires: "
                        ++ (case id_to_item_type it_id of
                                Just item_type ->
                                    itemTypeToString item_type

                                Nothing ->
                                    "Unknown"
                           )
            )


renderDislikes : ItemSentiments -> List (Element Msg)
renderDislikes item_types_desired =
    Dict.toList item_types_desired
        |> List.filter (Tuple.second >> (\trd -> trd <= 0.0))
        |> List.map
            (\( it_id, trd ) ->
                text <|
                    "Dislikes: "
                        ++ (case id_to_item_type it_id of
                                Just item_type ->
                                    itemTypeToString item_type

                                Nothing ->
                                    "Unknown"
                           )
            )


smallHeaderInGrid : UiOptions -> String -> InventorySortType -> Element Msg
smallHeaderInGrid uiOptions str sortType =
    el
        [ Font.size 10
        , Events.onClick (GotUiOptionsMsg <| ChangeInventorySortType sortType)
        , UI.noUserSelect
        , Border.rounded 2
        , Element.mouseOver [ Background.color UI.color_very_light_grey ]
        , padding 2
        , Element.pointer
        ]
    <|
        text
            (str
                ++ (if uiOptions.inventorySortType == sortType then
                        case uiOptions.inventorySortDir of
                            Descending ->
                                "▲"

                            Ascending ->
                                "▼"

                    else
                        ""
                   )
            )


renderCharacterDetails : UI.ColorTheme -> Character -> ItemDb -> ProgressUnlocks -> Bool -> Element Msg
renderCharacterDetails colorTheme character item_db progressUnlocks is_shop_context =
    let
        { char_id, held_items, held_gold } =
            character

        rendered_desires : List (Element Msg)
        rendered_desires =
            renderDesires character.item_types_desired

        rendered_dislikes : List (Element Msg)
        rendered_dislikes =
            renderDislikes character.item_types_desired

        render_single_action_log : ActionLog -> Element Msg
        render_single_action_log log =
            el [] (text <| action_log_to_str colorTheme item_db log)

        rendered_action_log_items : List (Element Msg)
        rendered_action_log_items =
            if List.length character.action_log > 0 then
                character.action_log
                    |> List.reverse
                    |> List.take 50
                    -- |> List.map (\log -> Lazy.lazy render_single_action_log log)
                    |> List.map render_single_action_log

            else
                [ text "No actions taken" ]

        rendered_action_log : List (Element Msg)
        rendered_action_log =
            [ column
                [ width fill
                , Element.clip
                , paddingXY 0 10
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
                [ UI.button <|
                    UI.TextParams
                        { buttonType = UI.Primary
                        , customAttrs = []
                        , onPressMsg = GotUiOptionsMsg <| ToggleHideNonZeroRows character.char_id
                        , textLabel =
                            if character.hide_zero_qty_inv_rows then
                                "Show Nonzero"

                            else
                                "Hide Nonzero"
                        , colorTheme = colorTheme
                        }
                , UI.button <|
                    UI.TextParams
                        { buttonType = UI.Secondary
                        , colorTheme = colorTheme
                        , customAttrs =
                            [ Html.Events.preventDefaultOn "contextmenu"
                                (Decode.succeed <| ( GotUiOptionsMsg <| CycleFilterDisplayedItemsBackward character.char_id character.displayedItemType, True ))
                                |> Element.htmlAttribute
                            ]
                        , onPressMsg = GotUiOptionsMsg <| CycleFilterDisplayedItemsForward character.char_id character.displayedItemType
                        , textLabel =
                            "Filter: "
                                ++ (case character.displayedItemType of
                                        Nothing ->
                                            "All"

                                        Just itemType ->
                                            itemTypeToString itemType
                                   )
                        }
                ]
            ]
    in
    row [ width fill ]
        (rendered_desires
            ++ rendered_dislikes
            ++ rendered_action_log
            ++ (if containsProgressUnlock UnlockedInventoryFilters progressUnlocks then
                    rendered_inventory_controls

                else
                    []
               )
        )


render_inventory_grid :
    Model
    -> String
    -> Character
    -> ShopTrends
    -> Maybe ( CharacterId, Item )
    -> ListContext
    -> (InventoryRecord -> Element Msg)
    -> Element Msg
render_inventory_grid { historical_shop_trends, item_db, colorTheme, uiOptions, progressUnlocks, communityFund } header character shop_trends hovered_item context controls_column =
    let
        { char_id, held_items, held_gold } =
            character

        is_shop_context =
            context == ShopItems

        is_player_context =
            context == InventoryItems

        { show_charts_in_hovered_item } =
            uiOptions

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
                                el
                                    [ Font.italic
                                    , alignRight
                                    , Font.color <|
                                        case colorTheme of
                                            BrightTheme ->
                                                UI.color_grey

                                            DarkTheme ->
                                                UI.convertColor Color.white
                                    , Font.size 12
                                    ]
                                <|
                                    text "Hold Shift for more"

                              else
                                Element.none
                            ]
                        , paragraph [] <|
                            [ text "Current Price: "
                            , UI.renderGp colorTheme (current_price item)
                            ]
                                ++ (if
                                        is_item_trending
                                            shop_trends.item_type_sentiment
                                            item
                                            && item.raw_gold_cost
                                            /= current_price item
                                    then
                                        [ text " (originally "
                                        , UI.renderGp colorTheme item.raw_gold_cost
                                        , text ")"
                                        ]

                                    else
                                        []
                                   )
                        , if show_charts_in_hovered_item then
                            el [ paddingXY 20 20 ] <| viewSingleItemTypeCharts historical_shop_trends item.item_type

                          else
                            Element.none
                        ]
                    )

            else
                Element.none
    in
    Element.column [ width fill, spacingXY 0 5, height fill ] <|
        [ Element.row [ UI.font_scaled 2, width fill ]
            [ Element.el [ UI.border_bottom 2 ] <| text header
            , text "   "
            , if not is_shop_context then
                row [ width fill, UI.font_scaled 1, centerX, spacingXY 10 0 ] <|
                    [ row [ centerX, width Element.shrink, spacingXY 10 0 ]
                        [ row [ Font.alignRight ]
                            [ text "Held: "
                            , UI.renderGp colorTheme held_gold
                            ]
                        , if is_player_context && character.held_blood > 0 then
                            row [ width fill ]
                                [ UI.renderBlood colorTheme character.held_blood
                                ]

                          else
                            Element.none
                        , row [ width fill ]
                            [ text <| "Community Fund: "
                            , UI.renderGp colorTheme communityFund
                            ]
                        ]
                    ]

              else
                Element.none
            ]
        ]
            ++ [ Lazy.lazy5 renderCharacterDetails colorTheme character item_db progressUnlocks is_shop_context
               ]
            ++ (if not is_shop_context && List.length character.action_log > 0 then
                    divider

                else
                    []
               )
            ++ [ inventoryGrid
                    colorTheme
                    uiOptions
                    shop_trends
                    character
                    context
                    progressUnlocks
                    controls_column
               ]


inventoryGrid : ColorTheme -> UiOptions -> ShopTrends -> Character -> ListContext -> ProgressUnlocks -> (InventoryRecord -> Element Msg) -> Element Msg
inventoryGrid colorTheme uiOptions shop_trends character context progressUnlocks controls_column =
    let
        mouse_hover_attrs : Item -> List (Element.Attribute Msg)
        mouse_hover_attrs item =
            [ Events.onMouseEnter <| GotUiOptionsMsg <| MouseEnterShopItem context ( character.char_id, item )
            , Events.onMouseLeave <| GotUiOptionsMsg <| MouseLeaveShopItem context ( character.char_id, item )
            ]

        sortFunc : InventoryRecord -> InventoryRecord -> Order
        sortFunc =
            case uiOptions.inventorySortType of
                SortByName ->
                    buildCompare (.item >> .name)

                SortByPrice ->
                    buildCompare (.item >> get_single_adjusted_item_cost shop_trends)

                SortByAvgPrice ->
                    buildCompare (.avg_price >> getPrice)

                SortByQuantity ->
                    buildCompare (.quantity >> getQuantity)

                SortByItemType ->
                    buildCompare (.item >> .item_type >> itemTypeToString)

                SortByItemDesc ->
                    buildCompare (.item >> .description)

        items : InventoryRecords
        items =
            (if character.hide_zero_qty_inv_rows then
                List.filter
                    (\{ quantity } -> getQuantity quantity > 0)
                    character.held_items

             else
                character.held_items
            )
                |> List.sortWith sortFunc
                |> (\irs ->
                        case character.displayedItemType of
                            Nothing ->
                                irs

                            Just item_type ->
                                List.filter
                                    (\ir -> ir.item.item_type == item_type)
                                    irs
                   )

        table_columns : List (Element.Column InventoryRecord Msg)
        table_columns =
            [ { header =
                    Lazy.lazy3 smallHeaderInGrid uiOptions "Name" SortByName
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
                            (text (UI.clipText item.name 25))
              }
            , { header =
                    Lazy.lazy3 smallHeaderInGrid uiOptions "Price" SortByPrice
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
                                [ UI.renderGp colorTheme <|
                                    get_single_adjusted_item_cost shop_trends item
                                ]
                                    ++ [ if context /= ShopItems && priceDiff /= 0 && getQuantity quantity /= 0 then
                                            let
                                                diffColor =
                                                    UI.colorFromInt priceDiff (UI.convertColor Color.green) UI.color_black UI.color_danger
                                            in
                                            el [ Font.size 12, Font.color diffColor ] <| text <| " (" ++ signedFromInt priceDiff ++ ")"

                                         else
                                            Element.none
                                       ]
              }
            , { header =
                    Lazy.lazy3 smallHeaderInGrid uiOptions "Avg Px" SortByAvgPrice
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
                                        UI.renderGp colorTheme <| getPrice avg_price

                                    else
                                        Element.none
              }
            , { header =
                    Lazy.lazy3 smallHeaderInGrid uiOptions "Qty." SortByQuantity
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
            , { header =
                    Lazy.lazy3 smallHeaderInGrid uiOptions "Item Type" SortByItemType
              , width = fillPortion 2
              , view =
                    if containsProgressUnlock UnlockedShopTrends progressUnlocks then
                        \{ item } ->
                            Element.el [ centerY ] <|
                                renderItemTypeWithTrend shop_trends item.item_type

                    else
                        \{ item } ->
                            Element.el [ centerY ] <|
                                renderItemTypeWithoutTrend item.item_type
              }
            , { header =
                    Lazy.lazy3 smallHeaderInGrid uiOptions "Item Desc." SortByItemDesc
              , width = fillPortion 3
              , view =
                    \{ item } -> el [ centerY ] <| text <| UI.clipText item.description 24
              }
            , { header = Lazy.lazy3 smallHeaderInGrid uiOptions "Controls" SortByName --noop basically
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
                                            [ UI.cssRule "opacity" "0.5", height fill ]

                                         else
                                            [ UI.cssRule "opacity" "1.0", height fill ]
                                        )
                                    <|
                                        el [ height fill, centerY ] <|
                                            col.view item
                        }
                    )
    in
    Element.table [ spacing 5 ]
        { data = items
        , columns = table_columns
        }


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


sortByInvRecName : InventoryRecord -> String
sortByInvRecName =
    .item >> .name


getPlayer : Characters -> Player
getPlayer (Characters { player, shop, others }) =
    player


getShop : Characters -> Shop
getShop (Characters { player, shop, others }) =
    shop


getShopCharacter : Shop -> Character
getShopCharacter (Shop shop) =
    shop


setShop : Shop -> Characters -> Characters
setShop shop (Characters { player, others }) =
    Characters { player = player, shop = shop, others = others }


getOthers : Characters -> List Character
getOthers (Characters { player, shop, others }) =
    others


float_to_percent : Float -> String
float_to_percent flt =
    flt * 100 |> floor |> String.fromInt |> (\str -> str ++ "%")


viewSingleItemTypeCharts :
    List ShopTrends
    -> ItemType
    -> Element Msg
viewSingleItemTypeCharts historical_shop_trends item_type =
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
                        |> C.named (itemTypeToString item_type)
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
            , CE.onMouseMove (GotUiOptionsMsg << OnTrendChartHover) (CE.getNearest CI.dots)
            , CE.onMouseLeave ((GotUiOptionsMsg << OnTrendChartHover) [])
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


viewShopTrendsChart :
    UI.Device
    -> List ShopTrends
    -> List (CI.One TrendChartDatum CI.Dot)
    -> Element Msg
viewShopTrendsChart device historical_shop_trends hovered_trend_chart =
    let
        chart_width =
            case device.class of
                UI.Desktop ->
                    -- -400 because the 200 padding on each side
                    (toFloat device.size.width - 400) * 0.9

                _ ->
                    toFloat device.size.width * 0.8

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
                    itemTypeToString item_type_
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
                        |> C.named (itemTypeToString item_type)
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
            , CE.onMouseMove (GotUiOptionsMsg << OnTrendChartHover) (CE.getNearest CI.dots)
            , CE.onMouseLeave ((GotUiOptionsMsg << OnTrendChartHover) [])
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
        [ width <| Element.px <| round chart_width
        , height <| Element.px (chart_height + 20)
        , paddingXY 20 0
        , centerX
        ]
    <|
        Element.html <|
            C.chart
                chart_attributes
                chart_elements


charIdMatches : CharacterId -> Character -> Bool
charIdMatches char_id_to_match { char_id } =
    char_id == char_id_to_match


getCharacter : Characters -> CharacterId -> Maybe Character
getCharacter (Characters { player, shop, others }) char_id =
    if charIdMatches char_id (getInnerPlayer player) then
        Just (getInnerPlayer player)

    else if charIdMatches char_id (getInnerShop shop) then
        Just (getInnerShop shop)

    else
        others
            |> List.filter (charIdMatches char_id)
            |> List.head


render_single_player_action_log : ItemDb -> PlayerActionLog -> Element Msg
render_single_player_action_log item_db player_action_log =
    paragraph []
        [ case player_action_log of
            WelcomeMessageActionLog ->
                text "Welcome to ItemShop!"

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
                            ((Maybe.withDefault "Unknown" <| Maybe.map itemTypeToString mb_item_type)
                                ++ " -- These became quite valuable."
                            )

                    EventLeastDesiredItemType mb_item_type ->
                        text <|
                            ((Maybe.withDefault "Unknown" <| Maybe.map itemTypeToString mb_item_type)
                                ++ " -- Nobody is interested in these anymore."
                            )

            TookSpecialActionTogglePauseAi ->
                text "Toggle Play/Pause"

            TookSpecialActionUnlockItem item_id ->
                text <| "Found an item: " ++ (lookup_item_id_default item_db item_id).name

            MonsterDeliveredItemToShop itemId ->
                text "A monster died, and left an item to the shop"
        ]


render_single_player_upgrade : UI.ColorTheme -> PlayerUpgrade -> Element Msg
render_single_player_upgrade colorTheme player_upgrade =
    case player_upgrade of
        AutomaticGPM lvl ->
            paragraph [] [ text "Income: ", UI.renderGp colorTheme lvl, text "/sec" ]

        AutomaticBPtoSP lvl ->
            paragraph []
                [ text "Bloodfeed lv"
                , text <| String.fromInt lvl
                , text ": "
                , UI.renderBlood colorTheme -lvl
                , text <| " +" ++ String.fromInt lvl
                , el [ Font.size 12 ] <| text "stamina"
                , text "/5sec"
                ]


playerUpgrades_display : UI.ColorTheme -> List PlayerUpgrade -> ProgressUnlocks -> Element Msg
playerUpgrades_display colorTheme playerUpgrades progressUnlocks =
    if containsProgressUnlock UnlockedUpgrades progressUnlocks && not (List.isEmpty playerUpgrades) then
        let
            header =
                el
                    [ UI.font_scaled 2, UI.border_bottom 2, alignTop ]
                    (text "Upgrades")

            renderedUpgrades =
                column [ paddingXY 0 10, spacing 5 ] <|
                    List.map
                        (render_single_player_upgrade colorTheme)
                        playerUpgrades
        in
        column [ height fill ] [ header, renderedUpgrades ]

    else
        Element.none


player_action_log_display : ItemDb -> List PlayerActionLog -> Element Msg
player_action_log_display item_db player_action_logs =
    column [ height fill ]
        ([ el [ UI.font_scaled 2, UI.border_bottom 2, alignTop ] <| text "Action Log" ]
            ++ [ column [ paddingXY 0 10, Element.spacing 4 ]
                    (player_action_logs
                        |> List.reverse
                        |> List.take 5
                        |> List.map (render_single_player_action_log item_db)
                        |> (\logs ->
                                if List.length logs < 5 then
                                    logs
                                        ++ List.repeat
                                            (5 - List.length logs)
                                            (paragraph [] [ text UI.blankChar ])

                                else
                                    logs
                           )
                    )
               ]
        )


showHideDebugInventoriesButton : ColorTheme -> List (Element.Attribute Msg) -> Bool -> Element Msg
showHideDebugInventoriesButton colorTheme attrs show_debug_inventories =
    let
        buttonText =
            if show_debug_inventories then
                "Hide Debug"

            else
                "Show Debug"
    in
    UI.button <|
        UI.TextParams
            { buttonType = UI.Danger
            , customAttrs = UI.defineHtmlId "show_debug_inventories" :: attrs
            , onPressMsg = GotUiOptionsMsg ToggleShowDebugInventories
            , textLabel = buttonText
            , colorTheme = colorTheme
            }


shopInventoryControls : ColorTheme -> Player -> ShopTrends -> InventoryRecord -> Element Msg
shopInventoryControls colorTheme (Player player) shop_trends { item, quantity, avg_price } =
    shop_buy_button
        colorTheme
        (get_single_adjusted_item_cost shop_trends item)
        player.held_gold
        { item = item, quantity = quantity, avg_price = avg_price }


playerInventoryControls : ColorTheme -> ( Bool, ShopTrends ) -> InventoryRecord -> Element Msg
playerInventoryControls colorTheme ( shiftIsPressed, shop_trends ) { item, quantity, avg_price } =
    let
        hasItemsToSell =
            getQuantity quantity >= 1
    in
    if not shiftIsPressed then
        shop_sell_button
            colorTheme
            hasItemsToSell
            { item = item, quantity = setQuantity 1, avg_price = avg_price }

    else
        UI.button <|
            UI.TextParams
                { buttonType = UI.Danger
                , customAttrs =
                    [ Element.transparent <| not hasItemsToSell
                    , width (fill |> Element.minimum 120)
                    ]
                , onPressMsg = SacrificeItem item
                , textLabel = "Sacrifice"
                , colorTheme = colorTheme
                }


quantityToStr : Quantity -> String
quantityToStr =
    getQuantity >> String.fromInt


getQuestTitle : QuestType -> String
getQuestTitle questType =
    case questType of
        SellAnyItem _ ->
            "Sell any Item!"

        EarnGold _ ->
            "Earn gold!"


questProgress : QuestType -> String
questProgress questType =
    case questType of
        EarnGold { current, target } ->
            quantityToStr current
                ++ "/"
                ++ quantityToStr target

        SellAnyItem { current, target } ->
            quantityToStr current
                ++ "/"
                ++ quantityToStr target


viewSingleQuest : Quest -> Element Msg
viewSingleQuest quest =
    case quest of
        IncompleteQuest { questType, questId } ->
            let
                questTitle =
                    getQuestTitle questType
            in
            case questType of
                SellAnyItem { current, target } ->
                    text <|
                        questTitle
                            ++ "\n"
                            ++ quantityToStr current
                            ++ "/"
                            ++ quantityToStr target

                EarnGold { current, target } ->
                    text <|
                        questTitle
                            ++ "\n"
                            ++ quantityToStr current
                            ++ "/"
                            ++ quantityToStr target

        CompleteQuest { questType, questId } cashedInStatus ->
            text <| "Completed quest!\n" ++ getQuestTitle questType


quests_display : UI.ColorTheme -> Quests -> ProgressUnlocks -> Element Msg
quests_display colorTheme quests progressUnlocks =
    column [ height fill ]
        [ column [ height fill ]
            ([ el [ UI.font_scaled 2, UI.border_bottom 2, alignTop ] <| text "Today's Quests" ]
                ++ [ case quests.dailyQuests of
                        [] ->
                            el [ paddingXY 0 10, spacing 5 ] <|
                                text "Nothing for now!"

                        _ ->
                            column [ paddingXY 0 10, spacing 5 ] <|
                                List.map viewSingleQuest quests.dailyQuests
                   ]
            )
        , if containsProgressUnlock UnlockedLifeQuests progressUnlocks then
            column [ height fill ]
                ([]
                    ++ [ el [ UI.font_scaled 2, UI.border_bottom 2, alignTop ] <|
                            text "Life Quests"
                       ]
                    ++ [ column [ paddingXY 0 10, spacing 5 ]
                            [ case quests.persistentQuests of
                                [] ->
                                    text "Go find a Life Quest"

                                persistentQuests ->
                                    column [ paddingXY 0 10, spacing 5 ] <|
                                        List.map viewSingleQuest persistentQuests
                            ]
                       ]
                )

          else
            Element.none
        ]


viewDayTimer : UI.ColorTheme -> TimeOfDay -> ItemDb -> Characters -> Time.Posix -> Element Msg
viewDayTimer colorTheme timeOfDay item_db characters ai_tick_time =
    let
        sharedAttrs =
            [ height fill
            , Border.shadow { offset = ( 2, 2 ), size = 0, blur = 1.0, color = UI.color_light_grey }
            ]

        fillingAttrs =
            [ Background.color UI.color_primary
            , Border.roundEach
                { defaultRounded
                    | topLeft = 5
                    , bottomLeft = 5
                }
            ]
                ++ sharedAttrs

        emptyAttrs =
            [ Background.color UI.color_white
            , Border.roundEach
                { defaultRounded
                    | topRight = 5
                    , bottomRight = 5
                }
            ]
                ++ sharedAttrs
    in
    column [ centerX, width fill ]
        [ el [ centerX, Font.underline, padding 10 ] <| text "Time of Day"
        , case timeOfDay.currentPhase of
            ActivePhase timeDayStarted { msSinceStartOfDay } ->
                let
                    dayElapsedRaw =
                        let
                            sinceStart =
                                toFloat msSinceStartOfDay

                            dayLength =
                                toFloat timeOfDay.dayLengthInMs
                        in
                        sinceStart / dayLength

                    dayElapsed =
                        dayElapsedRaw * 100
                in
                row
                    [ width fill
                    , height (Element.px 20)
                    , Element.inFront <|
                        el [ width fill ] <|
                            el
                                [ width (Element.shrink |> Element.minimum 50)
                                , height fill
                                , Font.center
                                , centerY
                                , centerX
                                , Font.color UI.color_black
                                , Font.glow UI.color_white 1
                                , Background.color UI.color_light_grey
                                , padding 2
                                , Border.rounded 5
                                ]
                            <|
                                text <|
                                    float_to_percent dayElapsedRaw
                    ]
                    [ row ([ width <| fillPortion (round <| dayElapsed) ] ++ fillingAttrs) []
                    , row ([ width <| fillPortion (round <| 100 - dayElapsed) ] ++ emptyAttrs) []
                    ]

            _ ->
                Element.none
        ]


viewShopPrepPhase : Model -> Element Msg
viewShopPrepPhase model =
    let
        header title =
            el [ UI.font_scaled 2, paddingXY 0 10, Font.underline ] <| text title
    in
    column [ width fill ]
        [ Element.el [ UI.font_scaled 3, UI.padding_bottom 10 ] <| text "Prep Phase"
        , column [ Font.size 16, spacingXY 0 20 ]
            [ paragraph [] [ el [ Font.italic ] <| text "You are about to begin a new day, running your shop the best you can." ]
            , paragraph [] [ text "Each day, the shop's wares change, opposing trades come and go, and who knows what else might happen." ]
            , paragraph []
                [ text <|
                    ("You may see one of "
                        ++ (model.item_db
                                |> Dict.filter (\_ idbr -> idbr.is_unlocked)
                                |> Dict.size
                                |> String.fromInt
                           )
                        ++ " unlocked items, among the "
                        ++ String.fromInt model.numItemsToStartDayWith
                        ++ " the Shop will be starting with today. Maybe you can find more?"
                    )
                ]
            , paragraph []
                [ text <|
                    ("There has been word that "
                        ++ (getOthers model.characters |> List.length |> String.fromInt)
                        ++ " other traders have come to trade."
                    )
                ]
            , paragraph []
                [ text <|
                    ("The day tomorrow will last: "
                        ++ (String.fromInt <| model.timeOfDay.dayLengthInMs // 1000)
                        ++ " seconds."
                    )
                ]
            , paragraph [] <|
                let
                    unpopularItemTypes =
                        model.shop_trends.item_type_sentiment
                            |> Dict.filter (\itemTypeIdStr trend -> trend < 1.0)
                            |> Dict.keys
                            |> List.filterMap id_to_item_type

                    popularItemTypes =
                        model.shop_trends.item_type_sentiment
                            |> Dict.filter (\itemTypeIdStr trend -> trend > 1.0)
                            |> Dict.keys
                            |> List.filterMap id_to_item_type
                in
                [ text <|
                    case String.join ", " <| List.map itemTypeToString unpopularItemTypes of
                        "" ->
                            ""

                        item_types ->
                            "The following types of things are less popular: "
                                ++ item_types
                                ++ ". "
                , text <|
                    case String.join ", " <| List.map itemTypeToString popularItemTypes of
                        "" ->
                            ""

                        item_types ->
                            "The following types of things are more popular: "
                                ++ item_types
                                ++ "."
                ]
            , column [ width fill ] <|
                [ header "Today's Quests" ]
                    ++ (case model.quests.dailyQuests of
                            [] ->
                                [ text "No quests today" ]

                            dailies ->
                                let
                                    questRender quest =
                                        case quest of
                                            IncompleteQuest { questType, questId } ->
                                                text <| getQuestTitle questType ++ " (" ++ questProgress questType ++ ")"

                                            CompleteQuest { questType, questId } cashedInStatus ->
                                                text <| "Completed!: " ++ getQuestTitle questType
                                in
                                List.map questRender dailies
                       )
            ]
        , el [ centerX, paddingXY 0 100 ] <|
            column []
                [ el [ Font.size 16, centerX, padding 10 ] <| text "Are you ready?"
                , UI.button <|
                    UI.TextParams
                        { buttonType = UI.Secondary
                        , colorTheme = model.colorTheme
                        , customAttrs = [ width (fill |> Element.minimum 200) ]
                        , onPressMsg = BeginDay
                        , textLabel = "Begin Day"
                        }
                ]
        ]


viewShopSummary : UI.ColorTheme -> PostPhaseData -> Quests -> Element Msg
viewShopSummary colorTheme postPhaseData quests =
    let
        getSoldNumFromItemDbRecord : ItemDbRecord -> Int
        getSoldNumFromItemDbRecord { trade_stats } =
            trade_stats.times_others_traded + trade_stats.times_you_sold

        getNumItemTypeSold itemType =
            List.foldl
                (\newIdbr totalTypeSold ->
                    if newIdbr.item.item_type == itemType then
                        case lookup_item_id postPhaseData.itemDbAtStart newIdbr.item.id of
                            Just origItemDbRecord ->
                                totalTypeSold
                                    + (getSoldNumFromItemDbRecord newIdbr - getSoldNumFromItemDbRecord origItemDbRecord)

                            Nothing ->
                                totalTypeSold

                    else
                        totalTypeSold
                )
                0
                (Dict.values postPhaseData.itemDbAtEnd)

        header title =
            el [ UI.font_scaled 2, paddingXY 0 10, Font.underline ] <| text title
    in
    column [ width fill, Font.size 16 ]
        [ Element.el [ UI.font_scaled 3, UI.padding_bottom 10 ] <| text "End of Day"
        , column [ spacingXY 0 20 ]
            [ el [ Font.italic ] <| text "You've finished the work day, sent away your gold and resources, and put your inventory into cold-storage."
            , text "Tomorrow you'll build up your inventory once again, solving a new goal."
            ]
        , row [ width fill, Element.spaceEvenly ]
            [ column [ alignTop, width (fillPortion 2) ] <|
                let
                    questRender quest =
                        case quest of
                            IncompleteQuest { questType, questId } ->
                                text <| "Failed: " ++ getQuestTitle questType ++ " (" ++ questProgress questType ++ ")"

                            CompleteQuest ({ questType, questId } as questData) cashedInStatus ->
                                row [ spacingXY 10 0 ]
                                    [ text <| "Completed!: " ++ getQuestTitle questType ++ " (" ++ questProgress questType ++ ")"
                                    , case cashedInStatus of
                                        QuestNotCashedIn ->
                                            UI.button <|
                                                UI.TextParams
                                                    { buttonType = UI.Primary
                                                    , customAttrs = []
                                                    , onPressMsg = CashInQuestType questData
                                                    , textLabel = "Cash In"
                                                    , colorTheme = colorTheme
                                                    }

                                        QuestCashedIn ->
                                            text "+1 gem!"
                                    ]
                in
                []
                    ++ [ header "Daily Quests" ]
                    ++ List.map questRender quests.dailyQuests
            , column [ width (fillPortion 1) ] <|
                []
                    ++ [ header "Stats (todo)"
                       , text <| (++) "Gold made: " <| String.fromInt <| max 0 <| postPhaseData.goldAtEndOfDay - postPhaseData.goldAtStartOfDay
                       , text <| "Number of all items sold:" ++ "???"
                       ]
                    ++ List.map
                        (\itemType ->
                            "Number of "
                                ++ itemTypeToString itemType
                                ++ " sold: "
                                ++ (String.fromInt <|
                                        getNumItemTypeSold itemType
                                   )
                                |> text
                        )
                        allItemTypes
                    ++ [ text <| "Item sold the most: " ++ "???"
                       , text <| "Most expensive item sold: " ++ "???"
                       , text <| "Most expensive item bought: " ++ "???"
                       ]
            ]
        , el [ centerX, paddingXY 0 100, width fill ] <|
            let
                columnStyle =
                    [ centerX, width (fill |> Element.maximum 200), alignBottom ]
            in
            row [ spacing 50, width fill, centerX ]
                [ column columnStyle
                    [ el [ centerX, padding 10 ] <|
                        paragraph [ Font.center ] [ text "Spend gems on permanent upgrades?" ]
                    , UI.button <|
                        UI.TextParams
                            { buttonType = UI.Primary
                            , colorTheme = colorTheme
                            , customAttrs = [ width (fill |> Element.minimum 200) ]
                            , onPressMsg = ToggleViewGemUnlocksInPostPhase
                            , textLabel = "Upgrades"
                            }
                    ]
                , column columnStyle
                    [ el [ centerX, padding 10 ] <|
                        text "End the day?"
                    , UI.button <|
                        UI.TextParams
                            { buttonType = UI.Secondary
                            , colorTheme = colorTheme
                            , customAttrs = [ width (fill |> Element.minimum 200) ]
                            , onPressMsg = EndDay
                            , textLabel = "Go to sleep"
                            }
                    ]
                ]
        ]


viewGemUnlocksInPostPhase : UI.ColorTheme -> ProgressUnlocks -> PostPhaseData -> Quantity -> Quests -> Element Msg
viewGemUnlocksInPostPhase colorTheme progressUnlocks postPhaseData heldGems quests =
    let
        columnStyle =
            [ centerX, width (fill |> Element.maximum 200), alignBottom ]

        progressUnlockButton : ProgressUnlock -> Element Msg
        progressUnlockButton progressUnlock =
            let
                price =
                    getProgressUnlockGemPrice progressUnlock

                alreadyHasUnlock =
                    containsProgressUnlock progressUnlock progressUnlocks

                canAfford =
                    getQuantity heldGems >= getPrice price

                buttonType =
                    if alreadyHasUnlock then
                        UI.Outline

                    else if canAfford then
                        UI.Primary

                    else
                        UI.Danger

                onPressMsg =
                    if alreadyHasUnlock then
                        Noop

                    else if canAfford then
                        UnlockProgressUnlock progressUnlock price

                    else
                        Noop
            in
            if not alreadyHasUnlock then
                UI.button <|
                    UI.CustomParams
                        { buttonType = buttonType
                        , colorTheme = colorTheme
                        , customAttrs = [ width (fill |> Element.minimum 200) ]
                        , onPressMsg = onPressMsg
                        , customLabel =
                            paragraph []
                                [ text <|
                                    progressUnlockToString progressUnlock
                                , text " ("
                                , if not canAfford then
                                    el [ Font.underline ] <|
                                        UI.renderGem colorTheme <|
                                            getPrice price

                                  else
                                    UI.renderGem colorTheme <| getPrice price
                                , text ")"
                                ]
                        }

            else
                Element.none
    in
    column [ width fill, Font.size 16, height fill ]
        [ Element.el [ UI.font_scaled 3, UI.padding_bottom 10 ] <| text "Unlocks"
        , column [ spacingXY 0 20 ]
            [ el [ Font.italic ] <| text "You've earned some gems. These will help the next day go a little smoother."
            , paragraph [] [ text "Each unlock is permanent. It might be cosmetic, it might be useless, it might be a whole new mechanic. Only one way to find out." ]
            ]
        , column [ paddingXY 0 10, spacing 5, width fill ]
            [ text "These are the things you can unlock:"
            , column [ width fill, spacing 5 ] <|
                (allProgressUnlocks
                    |> List.filter (\apu -> not <| List.member apu progressUnlocks)
                    |> List.Extra.greedyGroupsOf 3
                    |> List.map
                        (row [ spacing 5, centerX ]
                            << List.map progressUnlockButton
                        )
                )
            ]
        , column columnStyle
            [ el [ centerX, padding 10, alignBottom ] <|
                paragraph [ Font.center ] [ text "" ]
            , UI.button <|
                UI.TextParams
                    { buttonType = UI.Secondary
                    , colorTheme = colorTheme
                    , customAttrs = [ width (fill |> Element.minimum 200) ]
                    , onPressMsg = ToggleViewGemUnlocksInPostPhase
                    , textLabel = "Back"
                    }
            ]
        ]


viewShopPostPhase : ( UI.ColorTheme, Bool ) -> ProgressUnlocks -> PostPhaseData -> Characters -> Quests -> Element Msg
viewShopPostPhase ( colorTheme, shouldViewGemUpgradesInPostPhase ) progressUnlocks postPhaseData characters quests =
    if not shouldViewGemUpgradesInPostPhase then
        viewShopSummary colorTheme postPhaseData quests

    else
        let
            (Player player) =
                getPlayer characters
        in
        viewGemUnlocksInPostPhase colorTheme progressUnlocks postPhaseData (setQuantity player.held_gems) quests


debugTimeOfDayControls : Model -> Element Msg
debugTimeOfDayControls { colorTheme, timeOfDay, ai_tick_time, characters, item_db } =
    el [ width fill, paddingXY 0 10 ] <|
        row [ width fill, Element.spaceEvenly ]
            [ UI.button <|
                UI.TextParams
                    { buttonType = UI.Primary
                    , customAttrs = []
                    , onPressMsg = ChangeCurrentPhase PrepPhase
                    , textLabel =
                        case timeOfDay.currentPhase of
                            PrepPhase ->
                                "Currently PrepPhase"

                            _ ->
                                "Change to PrepPhase"
                    , colorTheme = colorTheme
                    }
            , UI.button <|
                UI.TextParams
                    { buttonType = UI.Primary
                    , customAttrs = []
                    , onPressMsg =
                        ChangeCurrentPhase
                            (ActivePhase
                                ai_tick_time
                                { goldAtStartOfDay =
                                    let
                                        (Player player) =
                                            getPlayer characters
                                    in
                                    player.held_gold
                                , msSinceStartOfDay = 0
                                , itemDbAtStart = item_db
                                }
                            )
                    , textLabel =
                        case timeOfDay.currentPhase of
                            ActivePhase _ _ ->
                                "Currently ActivePhase"

                            _ ->
                                "Change to ActivePhase"
                    , colorTheme = colorTheme
                    }
            , UI.button <|
                UI.TextParams
                    { buttonType = UI.Primary
                    , customAttrs = []
                    , onPressMsg =
                        ChangeCurrentPhase
                            (PostPhase
                                (case timeOfDay.currentPhase of
                                    ActivePhase timeDayStarted { itemDbAtStart, goldAtStartOfDay } ->
                                        { itemDbAtStart = itemDbAtStart
                                        , itemDbAtEnd = item_db
                                        , goldAtStartOfDay = goldAtStartOfDay
                                        , goldAtEndOfDay =
                                            let
                                                (Player player) =
                                                    getPlayer characters
                                            in
                                            player.held_gold
                                        }

                                    _ ->
                                        Debug.todo "cant really switch to PostPhase without being in ActivePhase first"
                                )
                            )
                    , textLabel =
                        case timeOfDay.currentPhase of
                            PostPhase _ ->
                                "Currently PostPhase"

                            _ ->
                                "Change to PostPhase"
                    , colorTheme = colorTheme
                    }
            ]


view_shop_currentTabType : Model -> Element Msg
view_shop_currentTabType model =
    let
        { historical_player_actions, colorTheme, timeOfDay, ai_updates_paused, ai_tick_time, characters, item_db, progressUnlocks, playerUpgrades, quests, showMineGpGained, uiOptions, historical_shop_trends, shop_trends } =
            model

        player : Player
        player =
            getPlayer characters

        playerChar : Character
        playerChar =
            case getPlayer characters of
                Player p ->
                    p

        shopChar : Character
        shopChar =
            case getShop characters of
                Shop s ->
                    s

        paused_border_attrs =
            [ Border.color
                (case colorTheme of
                    BrightTheme ->
                        UI.color_light_grey

                    DarkTheme ->
                        UI.color_grey
                )
            , Border.width 10
            , Border.dashed
            ]

        unpaused_border_attrs =
            [ Border.color
                (UI.defaultSolidColor colorTheme)
            , Border.width 10
            , Border.dashed
            ]

        tickSecondButton =
            UI.button <|
                UI.TextParams
                    { buttonType = UI.Secondary
                    , colorTheme = colorTheme
                    , customAttrs = []
                    , onPressMsg = ForceTickSecond
                    , textLabel = "Force Tick"
                    }
    in
    Element.el
        ([ width fill, padding 10 ]
            ++ (if ai_updates_paused then
                    paused_border_attrs

                else
                    unpaused_border_attrs
               )
        )
    <|
        Element.column
            [ width fill, UI.font_scaled 1, height fill ]
        <|
            [ row [ spacing 5, width fill ]
                [ -- codex button
                  if containsProgressUnlock UnlockedCodex progressUnlocks then
                    Element.link []
                        { url = "#items"
                        , label =
                            UI.button <|
                                UI.TextParams
                                    { buttonType = UI.Secondary
                                    , colorTheme = colorTheme
                                    , customAttrs = []
                                    , onPressMsg = ChangeTabType ItemsUnlockedTabType
                                    , textLabel = "View Codex"
                                    }
                        }

                  else
                    Element.none
                , -- charts
                  if containsProgressUnlock UnlockedCharts progressUnlocks then
                    UI.outline_button [] (GotUiOptionsMsg ToggleShowMainChart) <|
                        if uiOptions.show_main_chart then
                            "Hide Charts"

                        else
                            "Charts"

                  else
                    Element.none
                , -- color theme button
                  if containsProgressUnlock UnlockedDarkMode progressUnlocks then
                    UI.outline_button [ alignRight ] ToggleColorTheme <|
                        case colorTheme of
                            BrightTheme ->
                                "Darken"

                            DarkTheme ->
                                "Brighten"

                  else
                    Element.none
                ]
            , if uiOptions.show_main_chart && containsProgressUnlock UnlockedCharts progressUnlocks then
                Element.el [ paddingXY 0 10, width fill ] <| viewShopTrendsChart uiOptions.device historical_shop_trends uiOptions.hovered_trend_chart

              else
                Element.none
            , row [ width fill ] [ Lazy.lazy5 viewDayTimer colorTheme timeOfDay item_db characters ai_tick_time ]
            , row [ width fill, height <| Element.px 10 ] []
            , row [ width fill, spacingXY 10 0 ]
                [ el [ width <| fillPortion 3, alignTop ] <| Lazy.lazy2 player_action_log_display item_db historical_player_actions
                , el [ width <| fillPortion 6, alignTop ] <| Lazy.lazy3 playerUpgrades_display colorTheme playerUpgrades progressUnlocks
                , el [ width <| fillPortion 3, alignTop ] <|
                    Lazy.lazy3 quests_display colorTheme quests progressUnlocks
                ]
            , special_actions_display
                colorTheme
                progressUnlocks
                playerUpgrades
                uiOptions.hoveredTooltip
                playerChar
                ai_updates_paused
                showMineGpGained
            , if containsProgressUnlock UnlockedShopTrends progressUnlocks then
                trends_display
                    colorTheme
                    uiOptions.shiftIsPressed
                    item_db
                    shop_trends
                    characters
                    uiOptions.shop_trends_hovered

              else
                Element.none
            , Element.el [ paddingXY 0 0, width fill ] <|
                render_inventory_grid
                    model
                    "Items For Sale"
                    shopChar
                    shop_trends
                    uiOptions.hovered_item_in_character
                    ShopItems
                    (\ir -> Lazy.lazy4 shopInventoryControls colorTheme player shop_trends ir)
            , Element.el [ paddingXY 0 10, width fill ] <|
                render_inventory_grid
                    model
                    "Items In Inventory"
                    playerChar
                    shop_trends
                    uiOptions.hovered_item_in_character
                    InventoryItems
                    (playerInventoryControls colorTheme ( uiOptions.shiftIsPressed, shop_trends ))
            ]
                ++ [ column [ width fill, spacingXY 0 20 ] <|
                        []
                            ++ [ showHideDebugInventoriesButton
                                    colorTheme
                                    []
                                    uiOptions.show_debug_inventories
                               ]
                            ++ (if uiOptions.show_debug_inventories then
                                    [ tickSecondButton
                                    , debugTimeOfDayControls model
                                    , text <|
                                        UI.deviceClassToString uiOptions.device.class
                                            ++ " - "
                                            ++ UI.orientationToString uiOptions.device.orientation
                                    ]
                                        ++ (getOthers characters
                                                |> List.sortBy (.char_id >> UUID.toString)
                                                |> List.map
                                                    (\character ->
                                                        Element.Keyed.el [ height fill, paddingXY 0 10, width fill ]
                                                            ( UUID.toString character.char_id
                                                            , render_inventory_grid
                                                                model
                                                                (character.name ++ "'s Inventory")
                                                                character
                                                                shop_trends
                                                                uiOptions.hovered_item_in_character
                                                                CharacterItems
                                                                (always Element.none)
                                                            )
                                                    )
                                           )

                                else
                                    []
                               )
                   ]


render_item_db_item : UI.ColorTheme -> ItemDbRecord -> Element Msg
render_item_db_item colorTheme { item, trade_stats, is_unlocked } =
    column [ width (fill |> Element.maximum 150), height fill ]
        [ text <| item.name
        , row [ Font.size 12 ]
            [ if is_unlocked then
                Element.none

              else
                el [ Font.color UI.color_primary ] <| text "LOCKED"
            ]
        , row [ width fill, Font.size 14, spacingXY 10 0 ]
            [ itemTypeToString item.item_type
                |> text
            , item.raw_gold_cost
                |> UI.renderGp colorTheme
                |> el [ alignRight ]
            ]
        , row [ width fill, Font.size 12 ]
            [ text "Num Bought: "
            , trade_stats
                |> .times_you_bought
                |> String.fromInt
                |> String.padLeft 3 '\u{2003}'
                |> text
                |> UI.monospace [ alignRight ]
            ]
        , row [ width fill, Font.size 12 ]
            [ text "Num Sold: "
            , trade_stats
                |> .times_you_sold
                |> String.fromInt
                |> String.padLeft 3 '\u{2003}'
                |> text
                |> UI.monospace [ alignRight ]
            ]
        , row [ width fill, Font.size 12 ]
            [ text "Others' Trades: "
            , trade_stats
                |> .times_others_traded
                |> String.fromInt
                |> String.padLeft 3 '\u{2003}'
                |> text
                |> UI.monospace [ alignRight ]
            ]
        ]


view_items_unlocked_currentTabType : UI.ColorTheme -> ItemDb -> Element Msg
view_items_unlocked_currentTabType colorTheme item_db =
    let
        back_btn =
            Element.link []
                { url = "#shop"
                , label = UI.button <| UI.TextParams { buttonType = UI.Danger, customAttrs = [], onPressMsg = ChangeTabType ShopTabType, textLabel = "Back to Shop", colorTheme = colorTheme }
                }

        -- item_grid : Element Msg
        render_item_grid =
            List.map (render_item_db_item colorTheme)
                >> Element.wrappedRow [ width fill, spacing 20 ]

        filterItemDb filterFn =
            List.filter filterFn (Dict.values item_db)
    in
    column [ spacing 10, padding 20 ]
        [ text "Item Codex"
        , back_btn
        , render_item_grid <| filterItemDb .is_unlocked
        , render_item_grid <| filterItemDb (not << .is_unlocked)
        ]


getGoldGainedLabelMovementY : Animator.Timeline GoldGainedAnimation -> Float
getGoldGainedLabelMovementY timeline =
    Animator.move timeline <|
        \shouldShow ->
            case shouldShow of
                ShowGoldGainedAnimation seed ->
                    Animator.at 50
                        |> Animator.leaveSmoothly 0.5
                        |> Animator.arriveSmoothly 0.5

                HideGoldAnimation seed ->
                    Animator.at 50

                NoGoldAnimation ->
                    Animator.at 0
                        |> Animator.leaveSmoothly 0.5
                        |> Animator.arriveSmoothly 0.5


getGoldGainedAlpha : Animator.Timeline GoldGainedAnimation -> Float
getGoldGainedAlpha timeline =
    Animator.linear timeline <|
        \state ->
            Animator.at <|
                case state of
                    ShowGoldGainedAnimation seed ->
                        1.0

                    HideGoldAnimation seed ->
                        0.0

                    NoGoldAnimation ->
                        0.0


viewCurrenciesOverlay : UI.ColorTheme -> Player -> Float -> Float -> Element Msg
viewCurrenciesOverlay colorTheme (Player player) goldGainedLabelMovementY goldGainedAlpha =
    row []
        [ text "Held: "
        , el [ Element.inFront <| el [ Element.alpha goldGainedAlpha, Element.moveUp goldGainedLabelMovementY ] <| text "+123" ] <| UI.renderGp colorTheme <| player.held_gold
        , text " "
        , UI.renderBlood colorTheme <| player.held_blood
        , text " "
        , UI.renderGem colorTheme <| player.held_gems
        ]


viewSettingsOverlay : TabType -> Element Msg
viewSettingsOverlay currentTabType =
    el
        [ Element.pointer
        , Element.mouseOver [ Font.color UI.color_primary ]
        , Events.onMouseDown <|
            if currentTabType /= SettingsTabType then
                ChangeTabType SettingsTabType

            else
                ChangeTabType ShopTabType
        ]
    <|
        text "Settings"


viewOverlay : Model -> Element Msg
viewOverlay model =
    let
        overlayAttrs =
            [ UI.defaultBackgroundColor model.colorTheme
            , Border.color
                (case model.colorTheme of
                    BrightTheme ->
                        UI.color_ultra_light_grey

                    DarkTheme ->
                        UI.convertColor Color.lightCharcoal
                )
            , Border.width 1
            , Border.rounded 3
            , UI.pointerEventsAll
            , UI.noUserSelect
            , padding 10
            ]
    in
    el
        [ width fill
        , height fill
        , Font.size 12
        , UI.pointerEventsNone
        , padding 1
        , Element.inFront <|
            if model.uiOptions.shouldDisplayShowDebugInventoriesOverlay then
                el [ width fill, padding 10 ] <|
                    showHideDebugInventoriesButton model.colorTheme [ width fill ] model.uiOptions.show_debug_inventories

            else
                Element.none
        ]
    <|
        row
            [ width fill, height fill ]
            [ row
                (Font.alignLeft
                    :: Element.alignLeft
                    :: Element.alignBottom
                    :: overlayAttrs
                )
                [ viewSettingsOverlay model.currentTabType
                ]
            , row
                (Font.alignRight
                    :: Element.alignRight
                    :: Element.alignBottom
                    :: overlayAttrs
                )
                [ Lazy.lazy4 viewCurrenciesOverlay
                    model.colorTheme
                    (getPlayer model.characters)
                    (getGoldGainedLabelMovementY model.goldGainedTimeline)
                    (getGoldGainedAlpha model.goldGainedTimeline)
                ]
            ]


setDevice : Model -> UI.Device -> Model
setDevice ({ uiOptions } as model) device =
    { model | uiOptions = { uiOptions | device = device } }



-- convertColor Color.grey


view : Model -> Html.Html Msg
view model =
    let
        deviceClass =
            model.uiOptions.device.class
    in
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
        , Element.inFront <|
            if model.currentTabType /= TitleScreenTabType then
                viewOverlay model

            else
                Element.none
        , Element.htmlAttribute <| Html.Events.on "wheel" (Decode.succeed (GotUiOptionsMsg ScrollViewport))
        , Element.htmlAttribute <| Html.Events.on "scroll" (Decode.succeed (GotUiOptionsMsg ScrollViewport))
        , width fill
        , if deviceClass == UI.Desktop then
            paddingXY 200 20

          else
            padding 20
        , UI.defaultBackgroundColor model.colorTheme
        , UI.defaultFontColor model.colorTheme
        ]
    <|
        case model.currentTabType of
            TitleScreenTabType ->
                Lazy.lazy viewTitleScreen model

            ShopTabType ->
                case model.timeOfDay.currentPhase of
                    ActivePhase _ _ ->
                        Lazy.lazy view_shop_currentTabType model

                    PrepPhase ->
                        Lazy.lazy viewShopPrepPhase model

                    PostPhase postPhaseData ->
                        Lazy.lazy5 viewShopPostPhase ( model.colorTheme, model.shouldViewGemUpgradesInPostPhase ) model.progressUnlocks postPhaseData model.characters model.quests

            ItemsUnlockedTabType ->
                Lazy.lazy2 view_items_unlocked_currentTabType model.colorTheme model.item_db

            BattleTabType ->
                Element.map GotBattleMsg <|
                    case getPlayer model.characters of
                        Player player ->
                            Lazy.lazy Battle.view model.battleModel

            SettingsTabType ->
                viewSettingsTab model


viewSettingsTab : Model -> Element Msg
viewSettingsTab model =
    text "SETTINGS"


build_special_action_button : UI.ColorTheme -> UI.HoveredTooltip -> Character -> SpecialActionConfig -> Element Msg
build_special_action_button colorTheme hoveredTooltip character { action, title, tooltip_text, price } =
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

        tooltip_config : UI.TooltipConfig Msg
        tooltip_config =
            tooltip_text
                |> (\t ->
                        if price /= Free then
                            let
                                renderedCost =
                                    UI.renderGp colorTheme <| getPrice price
                            in
                            UI.buildTooltipElementConfig t
                                (column []
                                    [ text t
                                    , el [ centerX, padding 1, Font.color UI.color_grey ] <| text "___"
                                    , paragraph [ padding 10, centerX, Font.size 14 ]
                                        [ text "Costs: "
                                        , renderedCost
                                        ]
                                    ]
                                )
                                (GotUiOptionsMsg << GotTooltipMsg)

                        else
                            UI.buildTooltipTextConfig t (GotUiOptionsMsg << GotTooltipMsg)
                   )

        button_attrs =
            if is_disabled then
                [ Background.color UI.color_grey
                , Border.color UI.color_grey
                ]
                    ++ (if UI.hoveredTooltipMatchesId hoveredTooltip tooltip_config.tooltip_id then
                            [ Background.color <| rgb 0 0 0
                            , Border.color <| rgb 0 0 0

                            --required mouseOver to override the default buttons behaviour
                            , Element.mouseOver [ Background.color UI.color_light_grey, Border.color UI.color_light_grey ]
                            ]

                        else
                            []
                       )

            else
                []

        msg =
            if not is_disabled then
                OnSpecialAction action price

            else
                Noop
    in
    UI.primary_button_tooltip
        { buttonType = UI.Primary
        , colorTheme = colorTheme
        , customAttrs = button_attrs
        , onPressMsg = msg
        , textLabel = title
        }
        tooltip_config
        hoveredTooltip


viewTitleScreen : Model -> Element Msg
viewTitleScreen model =
    let
        scaling =
            Animator.linear model.titleScreenAnimationState <|
                \state ->
                    Animator.at <|
                        let
                            { class, orientation } =
                                model.uiOptions.device
                        in
                        case ( class, orientation ) of
                            ( UI.Desktop, _ ) ->
                                if state == HighTitle then
                                    1

                                else
                                    5

                            ( _, UI.Portrait ) ->
                                if state == HighTitle then
                                    3

                                else
                                    10

                            ( _, _ ) ->
                                if state == HighTitle then
                                    1

                                else
                                    5

        continueBtnMoveDown =
            Animator.linear model.titleScreenAnimationState <|
                \state ->
                    Animator.at <|
                        if state == HighTitle then
                            500

                        else
                            0
    in
    column [ width fill, height fill, centerX, centerY, Element.clip ]
        [ column
            [ centerX
            , centerY
            , Font.center
            , Element.scale scaling
            , Element.pointer
            , Events.onMouseDown ClickedTitleTextLabel
            , UI.noUserSelect
            ]
            [ el [ Font.size 14, centerX ] <|
                text "Our"
            , el [] <|
                text "Item Shop"
            , el [ Font.size 14, centerX ] <|
                text "wants "
            , el [ Font.size 12, centerX ] <|
                text "you "
            ]
        , column
            [ centerX
            , alignBottom
            , Font.size 128
            , Element.moveDown continueBtnMoveDown
            , Events.onMouseDown ClickedTitlePlayLabel
            , Element.pointer
            ]
            [ text "Play" ]
        ]


scale_increase_income_cost : Int -> Price
scale_increase_income_cost current_level =
    (20 + (5 * current_level * current_level) * 2) |> setPrice


scale_increase_bp_to_sp_cost : Int -> Price
scale_increase_bp_to_sp_cost current_level =
    (60 + (5 * current_level * current_level) * 2) |> setPrice


viewMineGpGained : Animator.Timeline MineAnimation -> Element Msg
viewMineGpGained showMineGpGained =
    let
        gpGainedMovementX : Float
        gpGainedMovementX =
            Animator.move showMineGpGained <|
                \shouldShow ->
                    case shouldShow of
                        ShowMineAnimation seed ->
                            Animator.at 50
                                |> Animator.leaveSmoothly 0.5
                                |> Animator.arriveSmoothly 0.5

                        HideMineAnimation seed ->
                            Animator.at 50

                        NoMineAnimation ->
                            Animator.at 0
                                |> Animator.leaveSmoothly 0.5
                                |> Animator.arriveSmoothly 0.5

        gpGainedMovementY : Float
        gpGainedMovementY =
            Animator.move showMineGpGained <|
                \shouldShow ->
                    case shouldShow of
                        ShowMineAnimation seed ->
                            Animator.at
                                (Random.step (Random.float -30 30) seed |> Tuple.first)

                        HideMineAnimation seed ->
                            Animator.at
                                (Random.step (Random.float -30 30) seed |> Tuple.first)

                        NoMineAnimation ->
                            Animator.at 0

        alpha : Float
        alpha =
            Animator.linear showMineGpGained <|
                \state ->
                    Animator.at <|
                        case state of
                            ShowMineAnimation seed ->
                                1.0

                            HideMineAnimation seed ->
                                0.0

                            NoMineAnimation ->
                                0.0
    in
    el [ Element.moveRight gpGainedMovementX, Element.moveUp gpGainedMovementY, Element.alpha alpha ] <| text "+1"


type alias SpecialActionConfig =
    { action : SpecialAction
    , title : String
    , tooltip_text : String
    , price : Price
    }


sacToggleAiPause : Bool -> SpecialActionConfig
sacToggleAiPause ai_updates_paused =
    { action = TogglePauseAi
    , title =
        if ai_updates_paused then
            "Resume"

        else
            "Pause"
    , tooltip_text = "You tap your medallion, and time comes to a halt.\n\nYou take a breath, and feel a weight off your shoulders. You'll take your time with things."
    , price = Free
    }


sacMine : SpecialActionConfig
sacMine =
    { action = Mine
    , title = "Mine"
    , tooltip_text = "Chip away at the mountain, hoping for a sliver of silver.\n\nHas a chance of giving you some GP."
    , price = Free
    }


sacSearch : SpecialActionConfig
sacSearch =
    { action = InviteTrader
    , title = "Invite Trader"
    , tooltip_text = "Invite a fellow Trader.\n\nThey may or may not have new wares you've never seen!"
    , price = setPrice 50
    }


sacHighDesire : SpecialActionConfig
sacHighDesire =
    { action = TriggerEvent (EventVeryDesiredItemType Nothing)
    , title = "Spread Good Rumour"
    , tooltip_text = "Sets a random Item Type to high value.\n\nSpreads a rumour that a given Item Type was the talk of the next town over."
    , price = setPrice 45
    }


sacLowDesire : SpecialActionConfig
sacLowDesire =
    { action = TriggerEvent (EventLeastDesiredItemType Nothing)
    , title = "Spread Bad Rumour"
    , tooltip_text = "Sets a random Item Type to low value.\n\nSpreads a rumour that a given Item Type has a surplus of sellers."
    , price = setPrice 45
    }


sacUnlockItem : SpecialActionConfig
sacUnlockItem =
    { action = UnlockItem
    , title = "Item Search"
    , tooltip_text = "Spend cash to hire a mercenary to seek out items.\n\nAllows for invited traders to have new items."
    , price = setPrice 25
    }


sacCommunityFund : SpecialActionConfig
sacCommunityFund =
    { action = CommunityFund
    , title = "Contribute"
    , tooltip_text = "You've always been a public member of the community. Add to the Community fund.\n\nAllows for invited traders to be able to afford finding new items."
    , price = communityFundCost
    }


sacIncreaseIncome : Int -> SpecialActionConfig
sacIncreaseIncome income_level =
    { action = IncreaseIncome
    , title = "Invest"
    , tooltip_text = "Invest in another business, earning more income.\n\nIncreases the gold you get per second."
    , price = scale_increase_income_cost income_level
    }


sacIncreaseBpToSp : Int -> SpecialActionConfig
sacIncreaseBpToSp bp_to_sp_level =
    { action = IncreaseBPtoSP
    , title = "Cut"
    , tooltip_text = "Cut deeper, using more of the blood to help yourself.\n\nIncreases the stamina your golem will regain per second, and the amount of blood you'll spend."
    , price = scale_increase_bp_to_sp_cost bp_to_sp_level
    }


special_actions_display : UI.ColorTheme -> ProgressUnlocks -> List PlayerUpgrade -> UI.HoveredTooltip -> Character -> Bool -> Animator.Timeline MineAnimation -> Element Msg
special_actions_display colorTheme progressUnlocks playerUpgrades hoveredTooltip player ai_updates_paused showMineGpGained =
    let
        specialButtonBuilder : SpecialActionConfig -> Element Msg
        specialButtonBuilder specialActionConfig =
            build_special_action_button colorTheme hoveredTooltip player specialActionConfig

        button_toggle_ai_pause : Element Msg
        button_toggle_ai_pause =
            specialButtonBuilder (sacToggleAiPause ai_updates_paused)

        button_mine : Element Msg
        button_mine =
            Lazy.lazy specialButtonBuilder sacMine

        button_battle =
            if containsProgressUnlock UnlockedBattles progressUnlocks then
                UI.button <|
                    UI.TextParams
                        { buttonType = UI.Primary
                        , colorTheme = colorTheme
                        , customAttrs = []
                        , onPressMsg = ChangeTabType BattleTabType
                        , textLabel = "To Battle!"
                        }

            else
                Element.none

        button_search =
            Lazy.lazy specialButtonBuilder sacSearch

        button_high_desire =
            Lazy.lazy specialButtonBuilder sacHighDesire

        button_low_desire =
            Lazy.lazy specialButtonBuilder sacLowDesire

        button_unlock_item =
            Lazy.lazy specialButtonBuilder sacUnlockItem

        button_community_fund =
            -- Lazy.lazy4 build_special_action_button colorTheme hoveredTooltip player sacCommunityFund
            Lazy.lazy specialButtonBuilder sacCommunityFund

        button_increase_income =
            let
                income_level =
                    List.foldl
                        (\u acc ->
                            case u of
                                AutomaticGPM lvl ->
                                    lvl

                                _ ->
                                    acc
                        )
                        1
                        playerUpgrades
            in
            specialButtonBuilder <|
                sacIncreaseIncome income_level

        button_increase_bp_to_sp =
            let
                bp_to_sp_level =
                    List.foldl
                        (\u acc ->
                            case u of
                                AutomaticBPtoSP lvl ->
                                    lvl

                                _ ->
                                    acc
                        )
                        1
                        playerUpgrades
            in
            specialButtonBuilder <|
                sacIncreaseBpToSp bp_to_sp_level

        hasUnlockedSpecialActions =
            containsProgressUnlock UnlockedSpecialActions progressUnlocks
    in
    column [ width fill, spacing 10, paddingXY 0 10 ]
        [ el [ UI.font_scaled 2, UI.border_bottom 2 ] <| text "Special Actions"
        , Element.wrappedRow [ width fill, spacingXY 20 0 ]
            [ Element.wrappedRow [ width <| fillPortion 1, spacingXY 10 10, alignTop ]
                [ button_toggle_ai_pause
                , button_mine
                , Lazy.lazy viewMineGpGained showMineGpGained
                , button_battle
                ]
            , if hasUnlockedSpecialActions then
                Element.wrappedRow [ width <| fillPortion 1, spacingXY 10 10, alignTop ]
                    [ button_increase_income
                    , button_increase_bp_to_sp
                    ]

              else
                Element.none
            , if hasUnlockedSpecialActions then
                Element.wrappedRow [ width <| fillPortion 4, spacingXY 10 10, alignTop ]
                    [ button_search
                    , button_unlock_item
                    , button_community_fund
                    , button_high_desire
                    , button_low_desire
                    ]

              else
                Element.none
            ]
        ]


natural0 : Fuzz.Fuzzer Int
natural0 =
    Fuzz.intRange 0 Random.maxInt


natural1 : Fuzz.Fuzzer Int
natural1 =
    Fuzz.intRange 1 Random.maxInt


positive : Fuzz.Fuzzer Int
positive =
    Fuzz.intRange 0 Random.maxInt


suite : Test
suite =
    let
        testDevice =
            UI.classifyDevice { width = 1920, height = 1080 }

        testTimeNowFlag =
            Time.millisToPosix 0
    in
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "root test suite"
        [ describe "encoders"
            [ test "ShopTrends encoding" <|
                \_ ->
                    let
                        encodedShopTrends : String
                        encodedShopTrends =
                            Encode.encode 0 (encodeShopTrends initial_shop_trends)
                    in
                    Expect.ok
                        (Decode.decodeString decodeShopTrends encodedShopTrends)
            , fuzz (Fuzz.intRange 1 Random.maxInt) "PlayerUpgrade encoding" <|
                \level ->
                    let
                        toEncode =
                            [ AutomaticGPM level, AutomaticBPtoSP level ]

                        decodedResult =
                            toEncode
                                |> Encode.list encodePlayerUpgrade
                                |> Encode.encode 0
                                |> Decode.decodeString
                                    (Decode.list decodePlayerUpgrade)
                    in
                    Expect.ok decodedResult
            , test "SecondsWaitedSince encoding" <|
                \_ ->
                    let
                        encodedSecondsWaitedSince : String
                        encodedSecondsWaitedSince =
                            Encode.encode 0 (encodeSecondsWaitedSince { lastSpRefill = 123 })
                    in
                    Expect.ok
                        (Decode.decodeString decodeSecondsWaitedSince encodedSecondsWaitedSince)
            , describe "basic character encoding/decoding" <|
                let
                    inputChar : Character
                    inputChar =
                        createCharacter (generateUuid "josh") "josh"
                            |> (\c -> { c | name = "mike" })
                            |> (\c ->
                                    { c
                                        | held_items =
                                            [ { item =
                                                    lookup_item_id_str_default item_db
                                                        "6b7e301d-ab12-5e81-acfc-547e63004ffa"
                                              , quantity = setQuantity 8
                                              , avg_price = setPrice 20
                                              }
                                            ]
                                    }
                               )

                    item_db =
                        initial_item_db

                    encodedChar : String
                    encodedChar =
                        Encode.encode 0 (encodeCharacter inputChar)

                    decodedCharResult : Result Decode.Error Character
                    decodedCharResult =
                        Decode.decodeString (decodeCharacter item_db) encodedChar
                in
                [ test "decode successfully"
                    (\_ -> Expect.ok decodedCharResult)
                , test "make sure they're the same thing"
                    (\_ ->
                        case decodedCharResult of
                            Err _ ->
                                Expect.fail "didnt decode right"

                            Ok decodedChar ->
                                Expect.equal inputChar decodedChar
                    )
                ]
            , test "encoding/decoding item type sentiments" <|
                \_ ->
                    let
                        emptyItemSentiments : ItemSentiments
                        emptyItemSentiments =
                            Dict.empty

                        encoded =
                            Encode.encode 0 (encodeItemSentiments emptyItemSentiments)
                    in
                    Expect.ok <| Decode.decodeString (decodeItemSentiments "invalid") encoded
            , test "encoding/decoding trend tolerance" <|
                \_ ->
                    let
                        emptyTrendTolerance : TrendTolerance
                        emptyTrendTolerance =
                            { buy = Dict.empty, sell = Dict.empty }

                        encoded =
                            Encode.encode 0 (encodeTrendTolerance emptyTrendTolerance)
                    in
                    Expect.ok <| Decode.decodeString decodeTrendTolerance encoded
            , describe "item db encoding/decoding"
                [ test "item db basic encoding"
                    (\_ ->
                        let
                            item_db =
                                initial_item_db
                        in
                        Expect.ok
                            (Encode.encode 0 (encodeItemDb item_db)
                                |> Decode.decodeString (decodeItemDb initial_item_db)
                            )
                    )
                , test "item db test empty, make sure its not adding anything"
                    (\_ ->
                        let
                            item_db =
                                Dict.empty

                            decodedItemDb =
                                item_db
                                    |> (\idb -> Encode.encode 0 (encodeItemDb idb))
                                    |> Decode.decodeString (decodeItemDb initial_item_db)
                                    |> Result.withDefault initial_item_db
                        in
                        Expect.equal 0 (Dict.size decodedItemDb)
                    )
                ]
            ]
        , describe "Basic math check for changing averages"
            [ test "Adding nothing changes nothing in average" <|
                \_ ->
                    let
                        orig_avg =
                            10
                    in
                    Expect.equal orig_avg (add_to_average orig_avg 1 0 0)
            , fuzz natural0 "Starting the average from nothing is just the number you add" <|
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
                    createCharacter (generateUuid "Test Character !!") "Testy McTested"

                test_character2 : Character
                test_character2 =
                    createCharacter (generateUuid "Second test character") "Testa Mysticles"

                test_model : Model
                test_model =
                    init testTimeNowFlag testDevice "" Nothing |> Tuple.first

                ( test_item, test_item_qty, test_avg_price ) =
                    ( lookup_item_id_str_default test_item_db "a41ae9d3-61f0-54f9-800e-56f53ed3ac98", Quantity 12, setPrice 9999 )

                ( test_item2, test_item_qty2, test_avg_price2 ) =
                    ( lookup_item_id_str_default test_item_db "c3c38323-1743-5a47-a8e3-bf6ec28137f9", Quantity 12, setPrice 9999 )
            in
            [ fuzz (Fuzz.map setQuantity <| Fuzz.intRange 0 ((Random.maxInt // 2) - 1)) "playerSoldItem marks the quest complete when you sell enough, and it doesn't go over" <|
                \targetQty ->
                    let
                        questId =
                            generateUuid "testquest123"

                        quests =
                            { dailyQuests =
                                [ IncompleteQuest
                                    { questType = SellAnyItem { current = setQuantity 0, target = targetQty }
                                    , questId = questId
                                    }
                                ]
                            , persistentQuests = []
                            }

                        updatedQuests =
                            playerSoldItem (addQuantityInt targetQty 100000) quests
                    in
                    Expect.equal
                        { dailyQuests =
                            [ CompleteQuest
                                { questType = SellAnyItem { current = targetQty, target = targetQty }
                                , questId = questId
                                }
                                QuestNotCashedIn
                            ]
                        , persistentQuests = []
                        }
                        updatedQuests
            , describe "onCashInQuest cashes in as expect" <|
                let
                    questId =
                        generateUuid "testquest123"

                    questData =
                        { questType = SellAnyItem { current = setQuantity 30, target = setQuantity 30 }
                        , questId = questId
                        }
                in
                [ test "cashing in a completely tracked and completed quest cashes" <|
                    \_ ->
                        let
                            quests =
                                { dailyQuests =
                                    [ CompleteQuest questData QuestNotCashedIn ]
                                , persistentQuests = []
                                }

                            resultModel : Model
                            resultModel =
                                onCashInQuest { test_model | quests = quests } questData
                        in
                        Expect.equal [] <|
                            List.filter (not << questIsCashedIn) resultModel.quests.dailyQuests
                , test "cashing in a completely tracked and but incomplete quest does not cash in" <|
                    \_ ->
                        let
                            quests =
                                { dailyQuests =
                                    [ IncompleteQuest questData ]
                                , persistentQuests = []
                                }

                            resultModel : Model
                            resultModel =
                                onCashInQuest { test_model | quests = quests } questData
                        in
                        Expect.equal [] <|
                            List.filter questIsCashedIn resultModel.quests.dailyQuests
                , test "cashing in a incompletely tracked and complete quest does not" <|
                    \_ ->
                        let
                            questData_ =
                                { questType = SellAnyItem { current = setQuantity 1, target = setQuantity 30 }
                                , questId = questId
                                }

                            quests =
                                { dailyQuests =
                                    [ CompleteQuest questData_ QuestNotCashedIn ]
                                , persistentQuests = []
                                }

                            resultModel : Model
                            resultModel =
                                onCashInQuest { test_model | quests = quests } questData
                        in
                        Expect.equal [] <|
                            List.filter questIsCashedIn resultModel.quests.dailyQuests
                ]
            , describe "ProgressUnlock "
                [ fuzz natural1 "paying for a ProgressUnlock removes gems" <|
                    \rawPrice ->
                        let
                            gemPrice =
                                setPrice rawPrice

                            testModel =
                                test_model.characters
                                    |> mapPlayer (\p -> { p | held_gems = rawPrice })
                                    |> (\c -> { test_model | characters = c })

                            newModel : Model
                            newModel =
                                update (UnlockProgressUnlock UnlockedCharts gemPrice) testModel
                                    |> Tuple.first
                        in
                        Expect.all
                            [ \m ->
                                Expect.equal 0 (getInnerPlayer <| getPlayer m.characters).held_gems
                            , \m ->
                                Expect.true
                                    "the unlock should be unlocked in the new model"
                                    (containsProgressUnlock
                                        UnlockedCharts
                                        m.progressUnlocks
                                    )
                            , \m ->
                                Expect.false
                                    "the unlock should not be present in the original"
                                    (List.member
                                        UnlockedCharts
                                        testModel.progressUnlocks
                                    )
                            ]
                            newModel
                ]
            , fuzz (Fuzz.map Random.initialSeed <| Fuzz.intRange 1 Random.maxInt) "updateActiveTimeOfDay replaces items in shop, and gets a non zero amount" <|
                \newGlobalSeed ->
                    let
                        (Shop shop) =
                            getShop test_model.characters

                        (Shop result_shop) =
                            getShop
                                (onPrepNewDay
                                    { test_model
                                        | globalSeed = newGlobalSeed
                                    }
                                ).characters
                    in
                    Expect.all
                        [ Expect.notEqual shop.held_items
                        , \held_items ->
                            Expect.greaterThan 0 <| List.length held_items
                        ]
                        result_shop.held_items
            , fuzz (Fuzz.map setQuantity <| Fuzz.intRange 1 Random.maxInt) "playerSoldItem does not mark the quest complete when you dont sell enough " <|
                \targetQty ->
                    let
                        questId =
                            generateUuid "testquest123"

                        quests =
                            { dailyQuests =
                                [ IncompleteQuest
                                    { questType =
                                        SellAnyItem
                                            { current = setQuantity 0, target = targetQty }
                                    , questId = questId
                                    }
                                ]
                            , persistentQuests = []
                            }

                        updatedQuests =
                            playerSoldItem (setQuantity 0) quests
                    in
                    Expect.equal
                        { dailyQuests =
                            [ IncompleteQuest
                                { questType = SellAnyItem { current = setQuantity 0, target = targetQty }
                                , questId = questId
                                }
                            ]
                        , persistentQuests = []
                        }
                        updatedQuests
            , fuzz (Fuzz.map Time.millisToPosix int) "fetching only works if there's enough community funds" <|
                \ai_tick_time ->
                    let
                        preUpdateRecord =
                            { shop_trends = initial_shop_trends
                            , character = test_character
                            , shop = Shop test_character --doesnt matter here
                            , communityFund = 0
                            , globalSeed = test_model.globalSeed
                            }

                        postUpdateRecord =
                            ai_fetch_item ai_tick_time test_item_db preUpdateRecord
                    in
                    Expect.all
                        [ \pur -> Expect.equal preUpdateRecord.communityFund pur.communityFund
                        , \pur -> Expect.equal preUpdateRecord.character.held_items pur.character.held_items
                        ]
                        postUpdateRecord
            , fuzz (Fuzz.map Time.millisToPosix int) "fetching items adds an item and reduces the community fund" <|
                \ai_tick_time ->
                    let
                        preUpdateRecord =
                            { shop_trends = initial_shop_trends
                            , character = test_character
                            , shop = Shop test_character --doesnt matter here
                            , communityFund = 100000
                            , globalSeed = test_model.globalSeed
                            }

                        postUpdateRecord =
                            ai_fetch_item ai_tick_time test_item_db preUpdateRecord
                    in
                    Expect.all
                        [ \pur ->
                            Expect.lessThan
                                preUpdateRecord.communityFund
                                pur.communityFund
                        , \pur ->
                            Expect.greaterThan
                                (List.length preUpdateRecord.character.held_items)
                                (List.length pur.character.held_items)
                        ]
                        postUpdateRecord
            , test "clipText test clips" <|
                \_ ->
                    Expect.equal "abc..." <| UI.clipText "abcdef" 3
            , test "clipText test doesnt clip" <|
                \_ ->
                    Expect.equal "abcdef" <| UI.clipText "abcdef" 30
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
                    case getPlayer test_model.characters of
                        Player orig_player ->
                            updateSpecialAction InviteTrader (setPrice 10) test_model
                                |> (\( new_model, _ ) ->
                                        case getPlayer new_model.characters of
                                            Player new_player ->
                                                Expect.equal (orig_player.held_gold - 10) <| new_player.held_gold
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
                            specialActionUnlockItem model

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
                            specialActionUnlockItem model

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
                    let
                        (Player player) =
                            getPlayer test_model.characters

                        new_test_model =
                            { test_model | playerUpgrades = [ AutomaticGPM 1 ] }
                    in
                    update_player new_test_model
                        |> (\m ->
                                let
                                    (Player updated_player) =
                                        getPlayer m.characters
                                in
                                Expect.equal
                                    (player.held_gold + 1)
                                    updated_player.held_gold
                           )
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
                    case getPlayer test_model.characters of
                        Player player ->
                            update_player { test_model | playerUpgrades = [ AutomaticGPM 10 ] }
                                |> (\m ->
                                        case getPlayer m.characters of
                                            Player updated_player ->
                                                Expect.equal (player.held_gold + 10) updated_player.held_gold
                                   )
            , fuzz int "AutomaticGPM doesn't go past 50" <|
                \to_add ->
                    case getPlayer test_model.characters of
                        Player player ->
                            update_player { test_model | playerUpgrades = [ AutomaticGPM to_add ] }
                                |> (\m ->
                                        case getPlayer m.characters of
                                            Player updated_player ->
                                                Expect.true
                                                    "Can't have more than 50 max gold"
                                                    (updated_player.held_gold <= 50)
                                   )
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
            , describe "AutomaticBPtoSP takes bp and converts to bp on a timer" <|
                let
                    newBattleModel =
                        Battle.increaseGolemStamina test_model.battleModel -110

                    secondsWaitedSince =
                        test_model.secondsWaitedSince

                    newTestModel =
                        { test_model
                            | battleModel = newBattleModel

                            -- means the next apply upgrade will trigger the timer
                            , secondsWaitedSince = { secondsWaitedSince | lastSpRefill = Battle.secondsRequiredForSpRefill }
                        }

                    (Player player) =
                        getPlayer newTestModel.characters
                in
                [ fuzz (Fuzz.intRange 1 10) "SP goes up" <|
                    \upgradeLevel ->
                        let
                            upgrader p =
                                applyUpgrade (AutomaticBPtoSP upgradeLevel) ( p, newTestModel )
                        in
                        let
                            expectedNewPlayer =
                                { player
                                    | held_blood = player.held_blood - (bloodCostForRefillSp * upgradeLevel)
                                }

                            intendedBattleModel =
                                Battle.increaseGolemStamina newBattleModel upgradeLevel

                            expectedPlayerAndModel : ( Character, Model )
                            expectedPlayerAndModel =
                                ( expectedNewPlayer
                                , replaceCharacter expectedNewPlayer { newTestModel | battleModel = intendedBattleModel }
                                )

                            ( resultPlayer, resultModel ) =
                                upgrader player
                        in
                        Expect.equal
                            (Battle.monsterMap
                                (.statStamina >> .curVal)
                                (expectedPlayerAndModel
                                    |> Tuple.second
                                    |> .battleModel
                                    |> .golem
                                )
                            )
                            (Battle.monsterMap
                                (.statStamina >> .curVal)
                                resultModel.battleModel.golem
                            )
                , fuzz (Fuzz.intRange 1 10) "BP goes down" <|
                    \upgradeLevel ->
                        let
                            expectedNewPlayer =
                                { player
                                    | held_blood = player.held_blood - (bloodCostForRefillSp * upgradeLevel)
                                }

                            intendedBattleModel =
                                Battle.increaseGolemStamina newBattleModel upgradeLevel

                            expectedPlayerAndModel : ( Character, Model )
                            expectedPlayerAndModel =
                                ( expectedNewPlayer
                                , replaceCharacter expectedNewPlayer
                                    { newTestModel | battleModel = intendedBattleModel }
                                )

                            ( resultPlayer, resultModel ) =
                                applyUpgrade (AutomaticBPtoSP upgradeLevel) ( player, newTestModel )
                        in
                        Expect.equal
                            (expectedPlayerAndModel
                                |> Tuple.first
                                |> .held_blood
                            )
                            resultPlayer.held_blood
                , fuzz (Fuzz.intRange 1 10) "BP goes down, but only as needed" <|
                    \upgradeLevel ->
                        let
                            expectedNewPlayer =
                                { player
                                    | held_blood = player.held_blood - (bloodCostForRefillSp * 1)
                                }

                            --decrease the golem's stamina by just one tick to make sure it is missing a small amount
                            -- NOTE this is the initial test_model, not newTestModel
                            newerBattleModel =
                                Battle.increaseGolemStamina test_model.battleModel -1

                            newerTestModel =
                                { newTestModel | battleModel = newerBattleModel }

                            expectedPlayerAndModel : ( Character, Model )
                            expectedPlayerAndModel =
                                let
                                    intendedBattleModel =
                                        Battle.increaseGolemStamina newerBattleModel upgradeLevel
                                in
                                ( expectedNewPlayer
                                , replaceCharacter expectedNewPlayer
                                    { newTestModel | battleModel = intendedBattleModel }
                                )

                            ( resultPlayer, resultModel ) =
                                applyUpgrade (AutomaticBPtoSP upgradeLevel) ( player, newerTestModel )
                        in
                        Expect.equal
                            (expectedPlayerAndModel
                                |> Tuple.first
                                |> .held_blood
                            )
                            resultPlayer.held_blood
                ]
            ]
        ]
