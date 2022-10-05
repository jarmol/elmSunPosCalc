module PairedValuesTests exposing (tests)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import Test exposing (..)


toTest : ( Int, Int ) -> Test
toTest (( input, expectedOutput ) as testCase) =
    Test.test (Debug.toString testCase) <|
        \() ->
            input
                |> Expect.equal expectedOutput


tests : Test
tests =
    [ ( 123, 999 )
    , ( 159, 438 )
    , ( 666, 666 )
    ]
        |> List.map toTest
        |> Test.describe "Integer tests"
