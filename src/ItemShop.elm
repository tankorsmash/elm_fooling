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
    }


type Msg
    = BootShop


type alias Model =
    { owned_items : List Item
    , items_for_sale : List Item
    }


initial_items_for_sale : List Item
initial_items_for_sale =
    [ { name = "Boots", item_type = Armor, gold_cost = 5 }
    , { name = "Sword", item_type = Weapon, gold_cost = 15 }
    , { name = "Dagger", item_type = Weapon, gold_cost = 10 }
    , { name = "Book of the Dead", item_type = Spellbook, gold_cost = 20 }
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
    { owned_items = [], items_for_sale = initial_items_for_sale }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BootShop ->
            ( model, Cmd.none )


render_item_type : ItemType -> Element.Element msg
render_item_type item_type =
    text <| item_type_to_pretty_string item_type


render_single_item_for_sale : Item -> Element.Element Msg
render_single_item_for_sale item =
    Element.row [ font_scaled 1 ]
        [ text item.name
        , text ": "
        , text <| String.fromInt item.gold_cost
        , text "gp -- "
        , render_item_type item.item_type
        ]


padding_bottom : Int -> Element.Attribute msg
padding_bottom pad =
    Element.paddingEach { bottom = pad, left = 0, right = 0, top = 0 }


view : Model -> Html.Html Msg
view model =
    let
        welcome_header =
            Element.el [ font_scaled 3, padding_bottom 5 ] <| text "Welcome to the Item Shop!"

        items_for_sale =
            Element.column [] <|
                [ Element.el [] <| text "Items for sale:" ]
                    ++ List.map render_single_item_for_sale model.items_for_sale
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        Element.column []
            [ welcome_header
            , items_for_sale
            ]


scaled : Int -> Int
scaled val =
    modular 16 1.25 val |> round


font_scaled : Int -> Element.Attribute msg
font_scaled scale =
    Font.size <| scaled scale
