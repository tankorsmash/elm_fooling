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
    { items : List Item
    }


init : Model
init =
    { items = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BootShop ->
            ( { items = [] }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        Element.row [] []
