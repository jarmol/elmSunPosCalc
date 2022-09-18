module DecimalTests exposing (..)

import Expect exposing (Expectation)
-- import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

degToRad = \x -> pi * x / 180.0


-- toTest : (Float, Float) -> Test
toTest ((input, expectedOutput) as testPoint) =
  Test.test (Debug.toString testPoint) <|
    \() ->
      input
        |> (-) expectedOutput |> abs 
        |> Expect.lessThan 1.00e-6

tests : Test
tests =
  [(pi, 3.14159)
  ,(degToRad 180, pi)
  ,(degToRad 90, pi / 2)
  ,(degToRad 60, pi / 3)
  ,(degToRad 45, pi / 4)
  ,(cos (pi/3), 0.50)
  ,( sin (pi/6), 0.50)
  ,(1111 / 1000, 1.111)]
  |> List.map toTest
  |> Test.describe "Trigonometric Functions Tests"
