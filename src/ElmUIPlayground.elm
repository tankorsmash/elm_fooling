module ElmUIPlayground exposing (Model, Msg, init, update, view)

import Element exposing (Element, alignLeft, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html


type Msg
    = Click


type alias Model =
    { rounded_edges : Int }


init : Model
init =
    { rounded_edges = 30 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )


view : Model -> Html.Html msg
view model =
    Element.layout [] <|
        myRowOfStuff model


myRowOfStuff : Model -> Element msg
myRowOfStuff model =
    let
        { rounded_edges } =
            model
    in
    row [ width fill, centerY, spacing 30 ]
        [ myElement rounded_edges "aa"
        , myElement rounded_edges "ss"
        , el [ alignRight ] <|
            row [ padding 20, spacing 20 ] (List.map (myElement rounded_edges) [ "asd", "ddd", "qweee" ])
        ]


myElement : Int -> String -> Element msg
myElement rounded_edges txt =
    el
        [ Background.color (rgb255 255 50 50)
        , Font.color (rgb255 255 255 255)
        , Border.rounded rounded_edges
        , padding 30
        ]
        (text txt)
