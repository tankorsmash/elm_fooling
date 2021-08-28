module ElmUIPlayground exposing (Model, Msg, init, update, view)

import Element exposing (Element, alignLeft, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html


type Msg
    = Click
    | Increment


type alias Model =
    { rounded_edges : Int }


init : Model
init =
    { rounded_edges = 3 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )
        Increment ->
            let old = Debug.log "old" model.rounded_edges
            in
            Debug.log "incremented!!!" ({model | rounded_edges = old +10}, Cmd.none)


view : Model -> Html.Html Msg
view model =
    Element.layout [] <|
        column [ width fill, centerY ]
            [ myRowOfStuff model
            , row []
                [ Input.button [] { onPress = (Just Increment), label = text "Click me" }
                ]
            ]


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
