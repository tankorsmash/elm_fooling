module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ItemShop exposing (add_to_average, sub_from_average)
import Test exposing (..)


suite : Test
suite =
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "Basic math check for changing averages"
        [ test "Adding nothing changes nothing in average" <|
            \_ ->
                let
                    orig_avg =
                        10
                in
                Expect.equal orig_avg (add_to_average orig_avg 1 0 0)
        , test "Adding a single item works" <|
            \_ ->
                let
                    orig_avg =
                        10

                    orig_num =
                        1
                in
                Expect.equal 15 (add_to_average orig_avg orig_num 20 1)
        , test "Removing nothing changes nothing in average" <|
            \_ ->
                let
                    orig_avg =
                        10
                in
                Expect.equal orig_avg (sub_from_average orig_avg 1 0 0)
        , test "Removing a single item changes nothing in average" <|
            \_ ->
                let
                    orig_avg =
                        20

                    orig_num =
                        2
                in
                Expect.equal 30 (sub_from_average orig_avg orig_num 10 1)
        ]
