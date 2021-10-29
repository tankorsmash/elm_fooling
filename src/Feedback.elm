module Feedback exposing (Model, Msg, init, subscriptions, update, view)

import Array
import Color
import Color.Convert as Convert
import DateFormat
import DateFormat.Relative
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
    { entries : List FeedbackEntry
    , time_now : Time.Posix
    , create_post : { title : Maybe String, details : Maybe String }
    }


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


type alias User =
    { username : String
    }


type alias Comment =
    { author : User
    , body : String
    , created_at : Time.Posix
    }


type alias FeedbackEntry =
    { title : String
    , body : String
    , votes : Votes
    , tags : List Tag
    , status : IssueStatus
    , created_at : Time.Posix
    , comments : List Comment
    }


type Msg
    = UpdateTime Time.Posix
    | CreatePostUpdateTitle String
    | CreatePostUpdateDetails String
    | CreatePostSubmit


initial_model : Model
initial_model =
    let
        initial_entries =
            [ { title = "I think we should change this"
              , body = "This is a long description of all the stuff we need to change, it's unreal. I am currently listening to Hell on Earth, and its boppin'.\n\nThis is a new line."
              , votes = { ups = 12, downs = 2 }
              , tags =
                    [ { name = "Feedback", description = "This is a tag for thoughts on a change" }
                    , { name = "Complaint", description = "This is a tag for all negative feelings" }
                    ]
              , status = InConsideration
              , created_at = Time.millisToPosix 1635544030000
              , comments = []
              }
            , { title = "Been lovin 3.0"
              , body = "Having a lot of fun playing the latest build, can't wait to see what you guys come up with next!\n\nI've been having a lot of fun listening to Beatiful Heartbeat by Morten, but of course remixed by Deoro. It's banging.\n\nIt's even better than the original, which is crazy."
              , votes = { ups = 2, downs = 30 }
              , tags =
                    [ { name = "Feedback", description = "This is a tag for thoughts on a change" }
                    , { name = "Complaint", description = "This is a tag for all negative feelings" }
                    ]
              , status = InConsideration
              , created_at = Time.millisToPosix 619663630000
              , comments = []
              }
            , { title = "Pretty sure I like PoGo more"
              , body = "I just love the amount of little digimons you pick up and put in your pocket. There isn't too much like it, so its a lot of fun."
              , votes = { ups = 13, downs = 10 }
              , tags =
                    [ { name = "Complaint", description = "This is a tag for all negative feelings" }
                    ]
              , status = InConsideration
              , created_at = Time.millisToPosix 1319262630000
              , comments = []
              }
            ]
    in
    { entries = initial_entries
    , time_now = Time.millisToPosix 0
    , create_post = { title = Nothing, details = Nothing }
    }


init : ( Model, Cmd Msg )
init =
    ( initial_model, Task.perform UpdateTime Time.now )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime new_time ->
            ( { model | time_now = new_time }, Cmd.none )

        CreatePostUpdateTitle new_title ->
            let
                { create_post } =
                    model
            in
            ( { model | create_post = { create_post | title = Just new_title } }, Cmd.none )

        CreatePostUpdateDetails new_details ->
            let
                { create_post } =
                    model
            in
            ( { model | create_post = { create_post | details = Just new_details } }, Cmd.none )

        CreatePostSubmit ->
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


clipText : String -> Int -> String
clipText str length =
    if String.length str > length then
        String.left length str ++ "..."

    else
        str


month_to_str : Time.Month -> String
month_to_str month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Fed"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


dateFormatter : Time.Zone -> Time.Posix -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]


format_date posix =
    dateFormatter Time.utc posix


format_relative_date time_now posix =
    DateFormat.Relative.relativeTime time_now posix



--biggest to smallest


format_date_raw posix =
    let
        zone =
            Time.utc

        year =
            String.fromInt <| Time.toYear zone posix

        month =
            month_to_str <| Time.toMonth zone posix

        day =
            String.fromInt <| Time.toDay zone posix

        hour =
            String.fromInt <| Time.toHour zone posix

        minutes =
            String.fromInt <| Time.toMinute zone posix

        seconds =
            String.fromInt <| Time.toSecond zone posix
    in
    String.join ""
        [ year
        , " "
        , month
        , " "
        , day
        , " - "
        , hour
        , ":"
        , minutes
        , ":"
        , seconds
        ]


render_entry : Time.Posix -> FeedbackEntry -> Element Msg
render_entry time_now entry =
    row [ spacing 25, width fill, padding 10 ]
        [ row [ width <| fillPortion 1 ]
            [ column [ centerX, Border.width 1, Border.rounded 4, padding 2, Border.color <| rgb 0.75 0.75 0.75, width fill ]
                [ el [ centerX ] <| text "/\\"
                , el [ centerX ] <| text <| String.fromInt <| entry.votes.ups - entry.votes.downs
                ]
            ]
        , column [ width <| fillPortion 13, spacing 10 ]
            [ el [ scaled_font 2 ] <| text entry.title
            , paragraph [ font_grey ] [ text <| clipText entry.body 150 ]
            , el [ Font.size 12 ] <| text <| format_date entry.created_at ++ " - " ++ format_relative_date time_now entry.created_at
            ]
        , row [ width <| fillPortion 1 ] [ text <| "[], " ++ (String.fromInt <| List.length entry.comments) ]
        ]


font_grey : Element.Attribute msg
font_grey =
    Font.color <| rgb 0.35 0.35 0.35


background_grey =
    Background.color <| rgb 0.96 0.96 0.96


border_dark_edges =
    Border.color <| rgb 0.937 0.937 0.937


background_white =
    Background.color <| rgb 1 1 1


thin_border =
    Border.width 1


view : Model -> Html.Html Msg
view model =
    let
        entries =
            model.entries

        render_entry_ entry =
            render_entry model.time_now entry

        input_style =
            [ border_dark_edges
            , thin_border
            , background_white
            , width fill
            , spacing 5
            , padding 10
            ]

        create_post_block =
            column [ background_grey, border_dark_edges, Border.width 1, padding 10, spacing 10, Border.rounded 5 ]
                [ el [ centerX ] <| text "Create Post"
                , column (font_grey :: input_style)
                    [ el [ Font.size 14 ] <| text "TITLE"
                    , Input.text [ Border.width 0 ]
                        { onChange = CreatePostUpdateTitle
                        , text =
                            case model.create_post.title of
                                Just title ->
                                    title

                                Nothing ->
                                    ""
                        , placeholder = Just <| Input.placeholder [] <| text "Short, descriptive title"
                        , label = Input.labelHidden "hidden title"
                        }
                    ]
                , column (font_grey :: input_style)
                    [ el [ Font.size 14 ] <| text "DETAILS"
                    , text "Any additional details"
                    ]
                , column [ border_dark_edges, thin_border, background_white, width fill ]
                    [ text "create post"
                    ]
                ]
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        column [ scaled_font 1 ] <|
            create_post_block
                :: List.map render_entry_ entries