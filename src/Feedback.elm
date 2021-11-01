module Feedback exposing (Model, Msg, Route, detail_view, init, subscriptions, update, view)

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
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
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
    , detail_entry_id : Maybe Int
    , detail_comment_body : Maybe String
    , detail_comment_focused : Maybe Int -- either an entry id or nothing
    , logged_in_user : Maybe User
    , users : List User
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


type Route
    = List
    | Detail Int


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
    , id : Int
    , author : User
    }


type Msg
    = UpdateTime Time.Posix
    | CreatePostUpdateTitle String
    | CreatePostUpdateDetails String
    | CreatePostSubmit
    | EntryDetailCommentUpdate String
    | EntryDetailCommentSubmit Int
    | EntryDetailCommentFocused Int
    | EntryDetailCommentLostFocused Int


initial_model : Model
initial_model =
    let
        mike =
            { username = "Jobang McDonald" }

        matt =
            { username = "Mathias Smith" }

        alice =
            { username = "Alicia Keys" }

        initial_users =
            [ mike, matt, alice ]

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
              , comments =
                    [ { author = mike
                      , body = "This is a short comment."
                      , created_at = Time.millisToPosix 1635543030000
                      }
                    , { author = alice
                      , body = "This however is quite a long comment. It turns out, while this is still simple, it still requires a bit of effort every step of the way. Kinda like life"
                      , created_at = Time.millisToPosix 1635543030000
                      }
                    ]
              , id = 1
              , author = mike
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
              , id = 2
              , author = matt
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
              , id = 3
              , author = alice
              }
            ]
    in
    { entries = initial_entries
    , time_now = Time.millisToPosix 0
    , create_post = { title = Nothing, details = Nothing }
    , detail_entry_id = Nothing
    , detail_comment_body = Nothing
    , detail_comment_focused = Nothing
    , logged_in_user = Just mike
    , users = initial_users
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
            case model.logged_in_user of
                Just logged_in_user ->
                    let
                        { create_post, entries } =
                            model

                        { title, details } =
                            create_post

                        new_entry =
                            { title = Maybe.withDefault "No title given" title
                            , body = Maybe.withDefault "No details given" details
                            , votes = { ups = 0, downs = 0 }
                            , tags = []
                            , status = New
                            , created_at = model.time_now
                            , comments = []
                            , id =
                                1
                                    + (case List.maximum <| List.map .id entries of
                                        Just max_id ->
                                            max_id

                                        Nothing ->
                                            0
                                      )
                            , author = logged_in_user
                            }

                        empty_create_post =
                            { title = Nothing, details = Nothing }
                    in
                    ( { model | entries = entries ++ [ new_entry ], create_post = empty_create_post }, Cmd.none )

                -- TODO: alert something because you need a logged in user
                Nothing ->
                    Debug.todo "need to alert the user that they have to be logged in"
                        ( model, Cmd.none )

        EntryDetailCommentUpdate new_comment ->
            ( { model | detail_comment_body = Just new_comment }, Cmd.none )

        EntryDetailCommentSubmit entry_id ->
            Debug.todo "gotta implement submitting a comment"
                ( model, Cmd.none )

        EntryDetailCommentFocused entry_id ->
            ( { model | detail_comment_focused = Just entry_id }, Cmd.none )

        EntryDetailCommentLostFocused entry_id ->
            ( { model | detail_comment_focused = Nothing }, Cmd.none )



-- ( { model | detail_comment_body = new_comment }, Cmd.none )


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
    row [ spacingXY 10 25, width fill, padding 10 ]
        [ row [ width <| fillPortion 1, width (fill |> Element.minimum 55), paddingXY 5 0, alignTop ]
            [ column [ alignTop, centerX, Border.width 1, Border.rounded 4, padding 2, Border.color <| rgb 0.75 0.75 0.75, width fill ]
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


font_white : Element.Attribute msg
font_white =
    Font.color <| rgb 1 1 1


background_grey =
    Background.color <| rgb 0.96 0.96 0.96


border_dark_edges =
    Border.color <| rgb 0.937 0.937 0.937


background_white =
    Background.color <| rgb 1 1 1


thin_border =
    Border.width 1


white_color =
    rgb 1 1 1


purple_color =
    rgb255 82 93 249


purple_button : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
purple_button attrs on_press label =
    Input.button
        ([ font_white
         , Font.size 16
         , Font.center
         , padding 6
         , Background.color purple_color
         , Border.rounded 5
         , Border.width 5
         , Border.color purple_color
         ]
            ++ attrs
        )
        { onPress = Just on_press, label = text label }


padding_left left =
    Element.paddingEach { top = 0, right = 0, bottom = 0, left = left }


explain_todo =
    Element.explain Debug.todo


render_single_detail : ( Maybe Int, Maybe String, Time.Posix ) -> FeedbackEntry -> Element Msg
render_single_detail ( detail_comment_focused_, detail_comment_body, time_now ) entry =
    let
        _ =
            123

        left_col_width =
            width <| Element.px 50

        left_padding =
            padding_left 50

        left_portion =
            width <| fillPortion 1

        right_portion =
            width <| fillPortion 9

        row_styling =
            [ spacing 10, padding 10, width fill ]

        left_blank =
            el [ left_portion ] <| Element.none

        detail_comment_focused =
            False
                || (case detail_comment_focused_ of
                        Nothing ->
                            False

                        Just entry_id ->
                            entry_id == entry.id
                   )

        render_comment : Comment -> Element Msg
        render_comment comment =
            row row_styling
                [ -- user image
                  el [ left_col_width, left_portion ] <|
                    el
                        [ Border.width 20
                        , Border.rounded 20
                        , Background.color <| rgb 0.2 0.3 0.4
                        , Border.color <| rgb 0.2 0.3 0.4
                        , Font.color <| white_color
                        , centerX
                        , height <| (fill |> Element.maximum 30)
                        , alignTop
                        , Element.inFront <|
                            el [ scaled_font 2, Font.center, alignTop, Element.moveUp 8, centerX ] <|
                                text <|
                                    String.left 1 comment.author.username
                        ]
                    <|
                        Element.none
                , column [ right_portion, spacing 10 ]
                    -- username
                    [ el [ left_portion, Font.semiBold ] <| text comment.author.username

                    --comment body
                    , el [ right_portion ] <| paragraph [] [ text comment.body ]
                    ]
                ]
    in
    column []
        ([ row row_styling
            -- vote box
            [ el [ left_col_width, left_portion ] <|
                row [ width (fill |> Element.minimum 55), paddingXY 5 0, alignTop ]
                    [ column [ alignTop, centerX, Border.width 1, Border.rounded 4, padding 2, Border.color <| rgb 0.75 0.75 0.75 ]
                        [ el [ centerX ] <| text "/\\"
                        , el [ centerX ] <| text <| String.fromInt <| entry.votes.ups - entry.votes.downs
                        ]
                    ]

            -- entry title
            , el [ right_portion, scaled_font 2 ] <| text <| entry.title
            ]
         , row (row_styling ++ [ paddingXY 10 0 ])
            [ -- user image
              el [ left_col_width, left_portion ] <|
                el
                    [ Border.width 20
                    , Border.rounded 20
                    , Background.color <| rgb 0.2 0.3 0.4
                    , Border.color <| rgb 0.2 0.3 0.4
                    , Font.color <| white_color
                    , centerX
                    , height <| (fill |> Element.maximum 30)
                    , alignTop
                    , Element.inFront <|
                        el [ scaled_font 2, Font.center, alignTop, Element.moveUp 8, centerX ] <|
                            text <|
                                String.left 1 entry.author.username
                    ]
                <|
                    Element.none

            -- username
            , el [ right_portion, Font.semiBold ] <| text entry.author.username
            ]
         , row row_styling
            [ -- blank space
              el [ left_portion ] <| Element.none

            -- entry body
            , column [ right_portion ]
                [ paragraph [] [ text <| entry.body ]
                , el [ Font.size 12, font_grey, paddingXY 0 10 ] <| text <| format_relative_date time_now entry.created_at
                , el
                    [ width fill
                    ]
                  <|
                    Input.multiline
                        ([ paddingXY 10 10
                         , width fill
                         , border_dark_edges
                         ]
                            ++ (if detail_comment_focused then
                                    [ Border.widthEach { top = 2, left = 2, right = 2, bottom = 0 }
                                    , Border.roundEach <| { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
                                    ]

                                else
                                    [ Border.width 2, Border.rounded 5 ]
                               )
                            ++ [ Events.onFocus <| EntryDetailCommentFocused entry.id
                               , Events.onLoseFocus <| EntryDetailCommentLostFocused entry.id
                               ]
                        )
                        { onChange = EntryDetailCommentUpdate
                        , text =
                            case detail_comment_body of
                                Just body ->
                                    body

                                Nothing ->
                                    ""
                        , placeholder = Just <| Input.placeholder [] <| text "Leave a comment"
                        , label = Input.labelHidden "hidden details"
                        , spellcheck = True
                        }
                , case detail_comment_focused of
                    True ->
                        el
                            [ border_dark_edges
                            , Border.width 2
                            , width fill
                            , paddingXY 10 10
                            ]
                        <|
                            purple_button
                                [ alignRight
                                , Font.variant Font.smallCaps
                                ]
                                (EntryDetailCommentSubmit entry.id)
                                "Submit"

                    False ->
                        Element.none
                ]
            ]
         , row row_styling
            [ left_blank
            , el [ right_portion, font_grey, Font.size 12, Font.medium ] <| text "ACTIVITY"
            ]
         ]
            ++ List.map render_comment entry.comments
        )


detail_view : Model -> Html.Html Msg
detail_view model =
    let
        { detail_entry_id, detail_comment_body, time_now, detail_comment_focused } =
            model
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [ scaled_font 1 ] <|
        case detail_entry_id of
            Just entry_id ->
                case List.head <| List.filter (.id >> (==) entry_id) model.entries of
                    Nothing ->
                        text <| "No entries match id given: " ++ String.fromInt entry_id

                    Just entry ->
                        render_single_detail ( detail_comment_focused, detail_comment_body, time_now ) entry

            Nothing ->
                text <| "No entry id given, 404"


view : Model -> Html.Html Msg
view model =
    let
        entries =
            model.entries

        render_entry_ entry =
            render_entry model.time_now entry

        input_style =
            [ border_dark_edges
            , Border.rounded 5
            , thin_border
            , background_white
            , width fill
            , spacing 5
            , padding 10
            , Font.alignLeft
            ]

        made_with_love =
            el [ centerX, paddingXY 0 10, font_grey, Font.size 12 ] <| text "Powered by Elm"

        create_post_block =
            column [ background_grey, border_dark_edges, Border.width 1, padding 10, spacing 10, Border.rounded 5, Element.below <| made_with_love ]
                [ el [ centerX ] <| text "Create Post"
                , column (font_grey :: input_style)
                    [ el [ Font.semiBold, Font.size 14 ] <| text "TITLE"
                    , Input.text [ Border.width 0, Font.alignLeft, paddingXY 0 10, spacing 0 ]
                        { onChange = CreatePostUpdateTitle
                        , text =
                            case model.create_post.title of
                                Just title ->
                                    title

                                Nothing ->
                                    ""
                        , placeholder = Just <| Input.placeholder [ alignLeft, Font.alignLeft ] <| text "Short, descriptive title"
                        , label = Input.labelHidden "hidden title"
                        }
                    ]
                , column (font_grey :: input_style)
                    [ el [ Font.semiBold, Font.size 14 ] <| text "DETAILS"
                    , Input.multiline [ Border.width 0, paddingXY 0 10, height (fill |> Element.minimum 75) ]
                        { onChange = CreatePostUpdateDetails
                        , text =
                            case model.create_post.details of
                                Just details ->
                                    details

                                Nothing ->
                                    ""
                        , placeholder = Just <| Input.placeholder [] <| text "Any additional details..."
                        , label = Input.labelHidden "hidden details"
                        , spellcheck = True
                        }
                    ]
                , column [ width fill ]
                    [ purple_button [ alignRight, Font.variant Font.smallCaps ] CreatePostSubmit "create post"
                    ]
                ]
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
        column [ scaled_font 1 ] <|
            [ row [ width fill ]
                [ el [ width <| fillPortion 1, alignTop ] create_post_block
                , column [ width <| fillPortion 5, spacingXY 0 20, alignTop ] <|
                    List.map render_entry_ entries
                ]
            ]
