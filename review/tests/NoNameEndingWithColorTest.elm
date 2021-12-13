module NoNameEndingWithColorTest exposing (all)

import NoNameEndingWithColor exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoNameEndingWithColor"
        [ test "should report an error when seeing a function or value ending with `_color`" <|
            \() ->
                """module SomeModule exposing (red_color)

red_color = 123"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Name should start with color_, not end with it"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
