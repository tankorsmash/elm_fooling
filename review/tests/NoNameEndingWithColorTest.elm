module NoNameEndingWithColorTest exposing (all)

import NoNameEndingWithColor exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoNameEndingWithColor"
        [ test "should report an error when seeing a function or value ending with `_color`" <|
            \() ->
                """module SomeModule exposing (nothing)

nothing = Nothing
red_color = 123"""
                    |> Review.Test.run rule
                    -- |> Review.Test.expectNoErrors
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Name should start with color_, not end with it"
                            , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                            , under = "red_color"
                            }
                            |> Review.Test.whenFixed """module SomeModule exposing (nothing)

nothing = Nothing
color_red = 123"""
                        ]
        , test "name with color in a module exposing should be replaced too" <|
            \() ->
                """module SomeModule exposing (red_color)

red_color = 123"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Name should start with color_, not end with it"
                            , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                            , under = "red_color"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } }
                            |> Review.Test.whenFixed """module SomeModule exposing (red_color)

color_red = 123"""
                        , Review.Test.error
                            { message = "Name should start with color_, not end with it"
                            , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                            , under = "red_color"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 29 }, end = { row = 1, column = 38 } }
                            |> Review.Test.whenFixed """module SomeModule exposing (color_red)

red_color = 123"""
                        ]
        ]
