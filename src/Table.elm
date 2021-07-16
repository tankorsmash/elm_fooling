module Table exposing (view)

import Html
    exposing
        ( Html
        , a
        , b
        , br
        , button
        , div
        , h1
        , h2
        , h3
        , h4
        , input
        , span
        , table
        , td
        , text
        , th
        , thead
        , tr
        )


view : Html msg
view =
    table []
        [ tr []
            [ th []
                [ td []
                    [ text "Header 1" ]
                ]
            , th []
                [ td []
                    [ text "Header 2" ]
                ]
            ]
        , tr []
            [ td [] [ text "value 1" ]
            , td [] [ text "value 2" ]
            ]
        ]
