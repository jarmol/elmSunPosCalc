module Jdn3Test exposing (jdnTest, jdnFuzzTest, jdnFuzz)

import GregorJDN exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Test exposing (..)

-- jdn = jdnGr 2022 8 16
-- expected 2459808
-- https://www.typecalendar.com/julian-date
-- https://elmprogramming.com/easy-to-test.html

-- 1. Some unit tests on the Julian Day Number JDN

jdnTest : Test
jdnTest =
   describe "JDN and JD for given date and time, "
       [ test "JDN of Date 2022-08-28" <|
           (\_ ->
               let year = 2022
                   month = 8
                   day = 28
                   expected = 2459820
               in jdnGr year month day
                  |> Expect.equal expected
           )
       , test "JD at 28 Aug 2022 10:24:31 UTC" <|
           (\_ ->
               let expected = 2459819.93369
                   epsilon  = 2.1e-6
               in  abs((jdateGr 2022 8 28 10 24 31) - expected)
                   |> Expect.atMost epsilon
           )
       , test "JDN of Date 2022-06-1 " <|
           (\_ ->
               let year = 2024
                   month = 6
                   day = 1
                   expected = 2460463
               in jdnGr year month day
                  |> Expect.equal expected
           )
       , test "Year 1999 length" <|
           (\_ ->
               let expected = 365
               in yearLength 1999
                  |> Expect.equal expected
           )
        , test "Length of the leap year 2000" <|
           (\_ ->
               let expected = 366 
               in yearLength 2000
                  |> Expect.equal expected
           )
        , test "Weekdays" <|
           (\_ ->
               let expected = "Friday"
               in weekday 2022 8 26
                  |> Expect.equal expected
           )
       ]


-- 2. Some fuzz tests on the Julian Day Number JDN
jdnFuzzTest : Test
jdnFuzzTest =
    describe "Variable days"
       [ fuzz (intRange 1 31) "2022 August 1 to 31"  <|
           (\day ->
                 let year = 2022
                     month = 8
                     start = 2459792
                 in  jdnGr year month day
                     |> Expect.equal (start + day)
           )
       ]


jdnFuzz : Test
jdnFuzz =
    describe "Variable day and year"
       [ fuzz2 (intRange 1 31) (intRange 2018 2027) "2018 - 2027, August" <|  
           (\day year ->
                 let month = 6 
                     start = 2459731 + (year - 2022) * 365 + qcor year
                 in  jdnGr year month day 
                     |> Expect.equal (start + day)
           )
       ]   

qcor : Int -> Int
qcor y =
    case y of
         2018 -> -1
         2019 -> -1
         2020 ->  0
         2021 ->  0
         2022 ->  0
         2023 ->  0
         _    ->  1


