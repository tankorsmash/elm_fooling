module Feedback exposing (Model, Msg, Route, detail_view, init, list_view, subscriptions, update)

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
import Set
import Task
import Time
import UUID exposing (UUID)


type alias Model =
    { entries : Array.Array FeedbackEntry
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
    { ups : List User
    , downs : List User
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
    , comments : Array.Array Comment
    , id : Int
    , author : User
    }


type Msg
    = UpdateTime Time.Posix
    | CreatePostUpdateTitle String
    | CreatePostUpdateDetails String
    | CreatePostSubmit
    | EntryDetailCommentUpdate String
    | EntryDetailCommentSubmit FeedbackEntry
    | EntryDetailCommentFocused Int
    | EntryDetailCommentLostFocused Int
    | EntryVotedUp Int


initial_model : Model
initial_model =
    let
        mike =
            { username = "Jobang McDonald" }

        matt =
            { username = "Mathias Smith" }

        alice =
            { username = "Alicia Keys" }

        mark =
            { username = "Marky Mark" }

        james =
            { username = "James Hetfield" }

        cliff =
            { username = "CliffyB" }

        initial_users =
            [ mike, matt, alice, mark, james, cliff ]

        initial_entries =
            Array.fromList
                [ { title = "I think we should change this"
                  , body = "This is a long description of all the stuff we need to change, it's unreal. I am currently listening to Hell on Earth, and its boppin'.\n\nThis is a new line."
                  , votes = { ups = [ mark, james, cliff ], downs = [ alice ] }
                  , tags =
                        [ { name = "Feedback", description = "This is a tag for thoughts on a change" }
                        , { name = "Complaint", description = "This is a tag for all negative feelings" }
                        ]
                  , status = InConsideration
                  , created_at = Time.millisToPosix 1635544030000
                  , comments =
                        Array.fromList
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
                  , votes = { ups = [ alice ], downs = [] }
                  , tags =
                        [ { name = "Feedback", description = "This is a tag for thoughts on a change" }
                        , { name = "Complaint", description = "This is a tag for all negative feelings" }
                        ]
                  , status = InConsideration
                  , created_at = Time.millisToPosix 619663630000
                  , comments = Array.empty
                  , id = 2
                  , author = matt
                  }
                , { title = "Pretty sure I like PoGo more"
                  , body = "I just love the amount of little digimons you pick up and put in your pocket. There isn't too much like it, so its a lot of fun."
                  , votes = { ups = [ james, mark, mike, cliff ], downs = [ alice ] }
                  , tags =
                        [ { name = "Complaint", description = "This is a tag for all negative feelings" }
                        ]
                  , status = InConsideration
                  , created_at = Time.millisToPosix 1319262630000
                  , comments = Array.empty
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
            if model.create_post.title == Nothing || model.create_post.details == Nothing then
                ( model, Cmd.none )

            else
                case model.logged_in_user of
                    Just logged_in_user ->
                        let
                            { create_post, entries } =
                                model

                            { title, details } =
                                create_post

                            new_entry : FeedbackEntry
                            new_entry =
                                { title = Maybe.withDefault "No title given" title
                                , body = Maybe.withDefault "No details given" details
                                , votes = { ups = [], downs = [] }
                                , tags = []
                                , status = New
                                , created_at = model.time_now
                                , comments = Array.empty
                                , id =
                                    1
                                        + (case List.maximum <| Array.toList <| Array.map .id entries of
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
                        ( { model
                            | entries = Array.push new_entry entries
                            , create_post = empty_create_post
                          }
                        , Cmd.none
                        )

                    -- TODO: alert something because you need a logged in user
                    Nothing ->
                        Debug.todo "need to alert the user that they have to be logged in"
                            ( model, Cmd.none )

        EntryDetailCommentUpdate new_comment ->
            ( { model | detail_comment_body = Just new_comment }, Cmd.none )

        EntryDetailCommentSubmit entry ->
            let
                comment_body =
                    case model.detail_comment_body of
                        Just body ->
                            body

                        Nothing ->
                            "Empty comment."
            in
            case model.logged_in_user of
                Just logged_in_user ->
                    let
                        new_comment : Comment
                        new_comment =
                            { author = logged_in_user, body = comment_body, created_at = model.time_now }

                        comments =
                            Array.push new_comment entry.comments

                        new_entry =
                            { entry | comments = comments }

                        new_entries =
                            Array.map
                                (\e ->
                                    if e.id == new_entry.id then
                                        new_entry

                                    else
                                        e
                                )
                                model.entries
                    in
                    ( { model
                        | entries = new_entries
                        , detail_comment_body = Nothing
                        , detail_comment_focused = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EntryDetailCommentFocused entry_id ->
            ( { model | detail_comment_focused = Just entry_id }, Cmd.none )

        EntryDetailCommentLostFocused entry_id ->
            if model.detail_comment_body == Nothing then
                ( { model | detail_comment_focused = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        EntryVotedUp entry_id ->
            case model.logged_in_user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    let
                        -- mb_entry = Array.get 0  <| Array.filter (.id >> ((==) entry_id)) model.entries
                        maybe_idx_entry =
                            List.head <| List.filter (Tuple.second >> .id >> (==) entry_id) <| Array.toIndexedList model.entries

                        new_model =
                            case maybe_idx_entry of
                                Just idx_entry ->
                                    -- TODO: find votes for logged in user, and then replace it, if needed
                                    let
                                        ( idx, entry ) =
                                            idx_entry

                                        { ups, downs } =
                                            entry.votes

                                        matches_username_ =
                                            username_matches user.username

                                        has_voted_up =
                                            List.any matches_username_ ups

                                        has_voted_down =
                                            List.any matches_username_ downs

                                        new_ups : List User
                                        new_ups =
                                            if has_voted_up then
                                                List.filter (not << matches_username_) ups

                                            else
                                                ups ++ [ user ]

                                        new_downs : List User
                                        new_downs =
                                            List.filter (not << matches_username_) downs

                                        new_votes : Votes
                                        new_votes =
                                            { ups = new_ups, downs = new_downs }

                                        replace_entry : FeedbackEntry -> FeedbackEntry
                                        replace_entry old_entry =
                                            if old_entry.id == entry.id then
                                                { old_entry | votes = new_votes }

                                            else
                                                old_entry

                                        new_entries =
                                            Array.map replace_entry model.entries
                                    in
                                    { model | entries = new_entries }

                                Nothing ->
                                    model
                    in
                    ( new_model, Cmd.none )



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


total_votes : Votes -> Int
total_votes votes =
    List.length votes.ups
        - List.length votes.downs


render_entry : Time.Posix -> FeedbackEntry -> Element Msg
render_entry time_now entry =
    let
        hovered_vote_style : List (Element.Attribute msg)
        hovered_vote_style =
            [ Element.mouseOver [ Border.color <| rgb 0 0 0 ] ]

        vote_block =
            row [ width <| fillPortion 1, width (fill |> Element.minimum 55), paddingXY 5 0, alignTop ]
                [ column
                    (hovered_vote_style
                        ++ [ Events.onClick <| EntryVotedUp entry.id
                           , Element.pointer
                           , alignTop
                           , centerX
                           , Border.width 1
                           , Border.rounded 4
                           , padding 2
                           , Border.color <| rgb 0.75 0.75 0.75
                           , width fill
                           ]
                    )
                    [ el [ centerX ] <| text "/\\"
                    , el [ centerX ] <|
                        text <|
                            String.fromInt <|
                                total_votes entry.votes
                    ]
                ]
    in
    row [ spacingXY 10 25, width fill, padding 10 ]
        [ vote_block
        , column [ width <| fillPortion 13, spacing 10 ]
            [ Element.link [ scaled_font 2 ] { url = "/feedback_tab/" ++ String.fromInt entry.id, label = text entry.title }
            , paragraph [ font_grey ] [ text <| clipText entry.body 150 ]
            , el [ Font.size 12 ] <| text <| format_date entry.created_at ++ " - " ++ format_relative_date time_now entry.created_at
            ]
        , row [ width <| fillPortion 1 ] [ text <| "[], " ++ (String.fromInt <| Array.length entry.comments) ]
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


username_matches : String -> User -> Bool
username_matches username user =
    user.username == username


render_single_detail : Model -> FeedbackEntry -> Element Msg
render_single_detail model entry =
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
                || (case model.detail_comment_focused of
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
                    , el [ right_portion ] <|
                        column [ spacing 10 ]
                            [ paragraph [] [ text comment.body ]
                            , el [ alignLeft, Font.size 14, font_grey ] <|
                                text <|
                                    "<3 1234 · "
                                        ++ format_relative_date model.time_now comment.created_at
                            ]
                    ]
                ]

        render_voter voter =
            row [ spacing 10 ]
                [ -- user image
                  el [ left_col_width, left_portion ] <|
                    el
                        [ Border.width 10
                        , Border.rounded 20
                        , Background.color <| rgb 0.2 0.3 0.4
                        , Border.color <| rgb 0.2 0.3 0.4
                        , Font.color <| white_color
                        , centerX
                        , height <| (fill |> Element.maximum 30)
                        , alignTop
                        , Element.inFront <|
                            el [ Font.size 16, Font.center, alignTop, Element.moveUp 8, centerX ] <|
                                text <|
                                    String.left 1 entry.author.username
                        ]
                    <|
                        Element.none
                , text voter.username
                ]

        participants : List User
        participants =
            let
                commenters : List User
                commenters =
                    Array.toList <| Array.map .author entry.comments

                voters : List User
                voters =
                    entry.votes.ups ++ entry.votes.downs

                includes_author =
                    Debug.log "includes author?" <| not <| List.isEmpty <| List.filter (\c -> c == entry.author) commenters

                everyone =
                    if includes_author then
                        commenters ++ voters

                    else
                        entry.author :: commenters ++ voters

                usernames : List String
                usernames =
                    Set.toList <| Set.fromList <| List.map .username everyone
            in
            List.filterMap
                (\username ->
                    List.head <|
                        List.filter (username_matches username) model.users
                )
            <|
                usernames

        voters_block =
            column
                [ alignTop
                , background_grey
                , border_dark_edges
                , Border.width 1
                , padding 20
                , spacing 10
                , Border.rounded 5
                , Element.below <| made_with_love
                ]
            <|
                ([ el [ centerX, font_grey, alignLeft, Font.size 12 ] <| text "PARTICIPANTS" ]
                    ++ List.map render_voter participants
                )
    in
    column []
        [ row
            [ paddingXY 0 5
            , width fill
            , Border.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
            , border_dark_edges
            ]
            [ Element.link [ padding 10 ]
                { url = "/feedback_tab", label = text "Home" }
            ]
        , row [ paddingXY 0 15 ]
            [ voters_block
            , column [ width fill ]
                ([ row row_styling
                    -- vote box
                    [ el [ left_portion, padding_left 3 ] <|
                        row []
                            [ column
                                [ alignTop
                                , centerX
                                , Border.width 1
                                , Border.rounded 4
                                , paddingXY 15 5
                                , Border.color <| rgb 0.75 0.75 0.75
                                ]
                                [ el [ centerX ] <| text "/\\"
                                , el [ centerX ] <|
                                    text <|
                                        String.fromInt <|
                                            total_votes entry.votes
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
                        , el [ Font.size 12, font_grey, paddingXY 0 10 ] <| text <| format_relative_date model.time_now entry.created_at
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
                                            [ Border.widthEach { top = 1, left = 1, right = 1, bottom = 0 }
                                            , Border.roundEach <| { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
                                            ]

                                        else
                                            [ Border.width 1, Border.rounded 5 ]
                                       )
                                    ++ [ Events.onFocus <| EntryDetailCommentFocused entry.id
                                       , Element.focused []
                                       , Events.onLoseFocus <| EntryDetailCommentLostFocused entry.id --TODO: fix this firing before the click on submitting the button
                                       ]
                                )
                                { onChange = EntryDetailCommentUpdate
                                , text =
                                    case model.detail_comment_body of
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
                                    , Border.width 1
                                    , width fill
                                    , paddingXY 10 10
                                    ]
                                <|
                                    purple_button
                                        [ alignRight
                                        , Font.variant Font.smallCaps
                                        ]
                                        (EntryDetailCommentSubmit entry)
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
                    ++ (Array.toList <| Array.map render_comment entry.comments)
                )
            ]
        ]


detail_view : Model -> Html.Html Msg
detail_view model =
    let
        { detail_entry_id, detail_comment_body, time_now, detail_comment_focused } =
            model
    in
    Element.layoutWith
        { options =
            [ Element.noStaticStyleSheet
            ]
        }
        [ scaled_font 1 ]
    <|
        case detail_entry_id of
            Just entry_id ->
                case Array.get 0 <| Array.filter (.id >> (==) entry_id) model.entries of
                    Nothing ->
                        text <| "No entries match id given: " ++ String.fromInt entry_id

                    Just entry ->
                        render_single_detail model entry

            Nothing ->
                text <| "No entry id given, 404"


made_with_love =
    el [ centerX, paddingXY 0 10, font_grey, Font.size 12 ] <| text "Powered by Elm"


list_view : Model -> Html.Html Msg
list_view model =
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

        create_post_block =
            column [ background_grey, border_dark_edges, Border.width 1, padding 10, spacing 10, Border.rounded 5, Element.below <| made_with_love ]
                [ el [ centerX, padding 10 ] <| text "Create a Post"
                , column (font_grey :: input_style)
                    [ el [ Font.semiBold, Font.size 14 ] <| text "TITLE"
                    , Input.text [ Border.width 0, Font.alignLeft, paddingXY 0 10, spacing 0, Element.focused [] ]
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
                    , Input.multiline [ Border.width 0, paddingXY 0 10, height (fill |> Element.minimum 75), Element.focused [] ]
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
                [ el [ width <| fillPortion 2, alignTop ] create_post_block
                , column [ width <| fillPortion 5, spacingXY 0 20, alignTop ] <|
                    Array.toList <|
                        Array.map render_entry_ entries
                ]
            ]
