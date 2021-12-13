module NoNameEndingWithColorTest exposing (all)

import NoNameEndingWithColor exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoNameEndingWithColor"
        [ test "should report an error when REPLACEME" <|
            \() ->
                """red_color = 123"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Rename the variable to color_XYZ"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
