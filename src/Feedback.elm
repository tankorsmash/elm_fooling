module Feedback exposing (Model, Msg, init, subscriptions, update, view)

import Array
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
import Task
import Time
import UUID exposing (UUID)


type alias Model =
    { entries : List FeedbackEntry }


type alias Tag =
    { name : String
    , description : String
    }


type IssueStatus
    = New
    | InConsideration
    | Rejected
    | Implemented


type alias Votes =
    { ups : Int
    , downs : Int
    }


type alias FeedbackEntry =
    { title : String
    , body : String
    , votes : Votes
    , tags : List Tag
    , status : IssueStatus
    }


type Msg
    = Boot


initial_model : Model
initial_model =
    let
        initial_entries =
            [ { title = "I think we should change this"
              , body = "This is a long description of all the stuff we need to change, it's unreal"
              , votes = { ups = 12, downs = 2 }
              , tags =
                    [ { name = "Feedback", description = "This is a tag for thoughts on a change" }
                    , { name = "Complaint", description = "This is a tag for all negative feelings" }
                    ]
              , status = InConsideration
              }
            , { title = "Been lovin 3.0"
              , body = "Having a lot of fun playing the latest build, can't wait to see what you guys come up with next!"
              , votes = { ups = 12, downs = 2 }
              , tags =
                    [ { name = "Feedback", description = "This is a tag for thoughts on a change" }
                    , { name = "Complaint", description = "This is a tag for all negative feelings" }
                    ]
              , status = InConsideration
              }
            ]
    in
    { entries = initial_entries
    }


init : ( Model, Cmd Msg )
init =
    ( initial_model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Boot ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


scaled : Int -> Int
scaled val =
    modular 16 1.25 val |> round


scaled_font : Int -> Element.Attribute msg
scaled_font scale =
    Font.size <| scaled scale


view : Model -> Html.Html Msg
view model =
    let
        entries =
            model.entries

        render_entry = .title >> text
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        column [ scaled_font 1 ] <|
            List.map render_entry entries
