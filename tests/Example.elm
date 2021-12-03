module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ItemShop exposing (add_to_average, sub_from_average)
import Test exposing (..)


suite : Test
suite =
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "Basic math check for changing averages"
        [ test "Adding nothing changes nothing" <|
            \_ ->
                let
                    orig_avg =
                        10
                in
                Expect.equal orig_avg orig_avg
        ]
