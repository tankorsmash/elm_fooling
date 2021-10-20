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
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html


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


type Msg
    = BootShop
    | MouseEnterShopItem Item
    | MouseLeaveShopItem Item


type alias Model =
    { owned_items : List Item
    , items_for_sale : List ( Item, Int )
    , hovered_item_for_sale : Maybe Item
    }


initial_items_for_sale : List ( Item, Int )
initial_items_for_sale =
    [ ( { name = "Boots", item_type = Armor, gold_cost = 5, description = "An old pair of boots" }, 3 )
    , ( { name = "Sword", item_type = Weapon, gold_cost = 15, description = "A rusted sword" }, 5 )
    , ( { name = "Dagger", item_type = Weapon, gold_cost = 10, description = "A small weapon that fits in your pocket" }, 4 )
    , ( { name = "Book of the Dead", item_type = Spellbook, gold_cost = 20, description = "Bound in leather, this book has a skull for a cover" }, 1 )
    ]


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
    { owned_items = [], items_for_sale = initial_items_for_sale, hovered_item_for_sale = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BootShop ->
            ( model, Cmd.none )

        MouseEnterShopItem item ->
            ( { model | hovered_item_for_sale = Just item }, Cmd.none )

        MouseLeaveShopItem item ->
            ( { model | hovered_item_for_sale = Nothing }, Cmd.none )


render_item_type : ItemType -> Element.Element Msg
render_item_type item_type =
    text <| item_type_to_pretty_string item_type


clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


render_single_item_for_sale : Maybe Item -> ( Item, Int ) -> Element.Element Msg
render_single_item_for_sale maybe_hovered_item ( item, qty ) =
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
                        , Border.color <| rgb 1 0 1
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
    in
    row
        [ font_scaled 1
        , width fill
        , Events.onMouseEnter <| MouseEnterShopItem item
        , Events.onMouseLeave <| MouseLeaveShopItem item
        , Element.below expanded_display
        ]
        [ column [ portion 2, font_scaled 2 ] [ text <| item.name ]
        , column [ portion 1 ] [ text <| String.fromInt item.gold_cost ++ "gp" ]
        , column [ portion 2 ] [ render_item_type item.item_type ]
        , column [ portion 1 ]
            [ if qty == 1 then
                text " "

              else
                text <| "x" ++ String.fromInt qty
            ]
        , column
            [ width <| (fillPortion 3 |> Element.maximum 300)
            ]
            [ text <| clipText item.description 40 ]
        ]


portion : Int -> Element.Attribute msg
portion =
    width << fillPortion


padding_bottom : Int -> Element.Attribute msg
padding_bottom pad =
    Element.paddingEach { bottom = pad, left = 0, right = 0, top = 0 }


view : Model -> Html.Html Msg
view model =
    let
        welcome_header =
            Element.el [ font_scaled 3, padding_bottom 5 ] <| text "Welcome to the Item Shop!"

        items_for_sale =
            Element.column [ width fill ] <|
                [ Element.el [] <| text "Items for sale:" ]
                    ++ List.map
                        (\item -> render_single_item_for_sale model.hovered_item_for_sale item)
                        model.items_for_sale
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        Element.column [ width fill ]
            [ welcome_header
            , items_for_sale
            ]


scaled : Int -> Int
scaled val =
    modular 16 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale
