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



--                         , Review.Test.error
--                             { message = "Names in signatures should start with color_, not end with it"
--                             , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
--                             , under = "red_color"
--                             }
--                             |> Review.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } }
--                             |> Review.Test.whenFixed """module SomeModule exposing (red_color)
--
-- color_red : Int
-- red_color = 123"""
--                         , Review.Test.error
--                             { message = "Function names should start with color_, not end with it"
--                             , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
--                             , under = "red_color"
--                             }
--                             |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 10 } }
--                             |> Review.Test.whenFixed """module SomeModule exposing (red_color)
--
-- red_color : Int
-- color_red = 123"""
--                         ]
--         , test "should report an error when seeing a function or value ending with `_color`" <|
--             \() ->
--                 """module SomeModule exposing (nothing)
--
-- nothing = Nothing
-- red_color = 123"""
--                     |> Review.Test.run rule
--                     |> Review.Test.expectErrors
--                         [ Review.Test.error
--                             { message = "Function names should start with color_, not end with it"
--                             , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
--                             , under = "red_color"
--                             }
--                             |> Review.Test.whenFixed """module SomeModule exposing (nothing)
--
-- nothing = Nothing
-- color_red = 123"""
--                         ]
--         , test "name with color in a module exposing should be replaced too" <|
--             \() ->
--                 """module SomeModule exposing (red_color)
--
-- red_color = 123"""
--                     |> Review.Test.run rule
--                     |> Review.Test.expectErrors
--                         [ Review.Test.error
--                             { message = "Function names should start with color_, not end with it"
--                             , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
--                             , under = "red_color"
--                             }
--                             |> Review.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } }
--                             |> Review.Test.whenFixed """module SomeModule exposing (red_color)
--
-- color_red = 123"""
--                         , Review.Test.error
--                             { message = "Exposed names should start with color_, not end with it"
--                             , details = [ "Having names start with color_ makes it easier to search for\nReplace `XYX_color` by `color_XYZ`" ]
--                             , under = "red_color"
--                             }
--                             |> Review.Test.atExactly { start = { row = 1, column = 29 }, end = { row = 1, column = 38 } }
--                             |> Review.Test.whenFixed """module SomeModule exposing (color_red)
--
-- red_color = 123"""
--                         ]
--      ]
