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


type alias Item =
    { name : String
    }


type Msg
    = BootShop


type alias Model =
    { owned_items : List Item
    , items_for_sale : List Item
    }


initial_items_for_sale : List Item
initial_items_for_sale =
    [ { name = "Boots" }
    , { name = "Sword" }
    , { name = "Dagger" }
    ]


init : Model
init =
    { owned_items = [], items_for_sale = initial_items_for_sale }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BootShop ->
            ( model, Cmd.none )


render_single_item_for_sale : Item -> Element.Element Msg
render_single_item_for_sale item =
    Element.row [] [ text item.name ]


view : Model -> Html.Html Msg
view model =
    let
        welcome_header =
            Element.el [ Font.size <| scaled 3 ] <| text "Welcome to the Item Shop!"

        items_for_sale =
            Element.column []
                [ Element.el [ Font.size <| scaled 2 ] <| text "Items for sale:"
                , Element.row [] <|
                    List.map render_single_item_for_sale model.items_for_sale
                ]
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        Element.column []
            [ welcome_header
            , items_for_sale
            ]


scaled : Int -> Int
scaled val =
    modular 16 1.25 val |> round
