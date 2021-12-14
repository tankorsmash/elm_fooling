module NoNameEndingWithColorTest exposing (all)

import NoNameEndingWithColor exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoNameEndingWithColor"
        [ test "names with colors should have its signature changed too" <|
            \() ->
                """module SomeModule exposing (red_color)

red_color : Int
red_color = 123"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed names should start with color_, not end with it"
                            , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                            , under = "red_color"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 29 }, end = { row = 1, column = 38 } }
                            |> Review.Test.whenFixed """module SomeModule exposing (color_red)

color_red : Int
color_red = 123"""
                        ]
        , test "individual functions should be fixed together" <|
            \() ->
                """module SomeModule exposing (red_color, green_color)

red_color : Int
red_color = 123
green_color : Int
green_color = 932133"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed names should start with color_, not end with it"
                            , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                            , under = "red_color"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 29 }, end = { row = 1, column = 38 } }
                            |> Review.Test.whenFixed """module SomeModule exposing (color_red, green_color)

color_red : Int
color_red = 123
green_color : Int
green_color = 932133"""
                        , Review.Test.error
                            { message = "Exposed names should start with color_, not end with it"
                            , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                            , under = "green_color"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 40 }, end = { row = 1, column = 51 } }
                            |> Review.Test.whenFixed """module SomeModule exposing (red_color, color_green)

red_color : Int
red_color = 123
color_green : Int
color_green = 932133"""
                        ]
        , test "renamed functions will be renamed everywhere" <|
            \() ->
                """module SomeModule exposing (red_color)

red_color : Int
red_color = 123
my_copy = red_color"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed names should start with color_, not end with it"
                            , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
                            , under = "red_color"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 29 }, end = { row = 1, column = 38 } }
                            |> Review.Test.whenFixed """module SomeModule exposing (color_red)

color_red : Int
color_red = 123
my_copy = color_red"""
                        ]
        , test "Should not report an error at all" <|
            \() ->
                """module SomeModule exposing (nothing, color_purple)

nothing = Nothing
color_green = 123

color_primary : Color
color_primary = rgb 0 0 0

color_purple = rgba 0 0 1 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
