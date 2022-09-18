module EquinoxDeclinationTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (Model,sunDeclination)
import Zip exposing (zip)
{-
Calculation and testing of the solar Declination in autumn equinox
Here is calculated the exact time of the next equinox on September 23. this year.
The equinoxes mean the moments when the rotation axis of Earth has tilt angle 0 in respect to Sun.
This happens twice each year - in autumn six months later in the spring. The tilt angle is called solar declination.
After autumn equinox of the declination is decreasing in the northern hemisphere and reaching the minimum during the winter solstice around December 21.
-}

minutes1 = [46,49,52,55,58]       -- for times 0:46 to 0:58 UTC

minutes2 = [1,4,7,10,13,16,19,22] -- for times 1:01 to 1:22

-- London 2022-09-23 autumn equinox at 01:04:27
london =  Model "2022" "9" "23" "1" "4" "27" "51.5" "-0.13"  "1"

mods1 = List.map (\mn -> {london | hour = "0", minute = String.fromInt mn}) minutes1

mods2 = List.map (\mn -> {london | hour = "1", minute = String.fromInt mn}) minutes2

models = List.append mods1 mods2

equinoxDeclinations = List.map sunDeclination models

declinationsNOAA = 
     [ 0.00486460929
     , 0.00405377463
     , 0.00324293884
     , 0.00243210229
     , 0.00162126479
     , 0.00081042635
     ,-0.0000004132
     ,-0.0008112535
     ,-0.0016220948
     ,-0.0024329370
     ,-0.0032437803
     ,-0.0040546244
     ,-0.0048654694
     ]

testPairs = Zip.zip equinoxDeclinations declinationsNOAA

toTest ((input, expectedOutput) as testPoint) =
  Test.test (Debug.toString testPoint) <|
    \() ->
      input
        |> (-) expectedOutput |> abs
        |> Expect.lessThan 4.60e-8

tests : Test
tests =
   testPairs
    |> List.map toTest
    |> Test.describe "Autumn Equinox Declination Tests"

-- List.minimum <| List.map abs equinoxDeclinations
-- Just 3.671600284714206e-7 : Maybe Float

-- List.minimum <| List.map abs declinationsNOAA
-- Just 4.132e-7 : Maybe Float

