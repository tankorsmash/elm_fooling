module ElmUIPlayground exposing (view, Msg, update, init, Model)

import Element exposing (Element, alignLeft, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html

type Msg = Click

type Model = Model Int

init : Model
init = Model 1

update :  Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Click -> (model, Cmd.none)

view : Html.Html msg
view =
    Element.layout []
        myRowOfStuff


myRowOfStuff =
    row [ width fill, centerY, spacing 30 ]
        [ myElement "aa"
        , myElement "ss"
        , el [ alignRight ]
            <| row [padding 20, spacing 20] (List.map myElement [ "asd", "ddd", "qweee" ])
        ]


myElement : String -> Element msg
myElement txt =
    el
        [ Background.color (rgb255 255 50 50)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text txt)
