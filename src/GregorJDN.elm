module GregorJDN exposing (jdateGr, jdnGr)


jdnGr : Int -> Int -> Int -> Int
jdnGr y m d =
    (1461 * (y + 4800 + (m - 14) // 12))
        // 4
        + (367 * (m - 2 - 12 * ((m - 14) // 12)))
        // 12
        - ((3 * ((y + 4900 + (m - 14) // 12) // 100)) // 4)
        + d
        - 32075



{- yearLength : Int -> Int
   yearLength y =
       jdnGr (y + 1) 1 1 - jdnGr y 1 1

   weekday : Int -> Int -> Int -> String
   weekday y m d =
       let
           dayNumber : Int
           dayNumber =
               remainderBy 7 (1 + jdnGr y m d)
       in
       case dayNumber of
           0 ->
               "Sunday"

           1 ->
               "Monday"

           2 ->
               "Tuesday"

           3 ->
               "Wednesday"

           4 ->
               "Thursday"

           5 ->
               "Friday"

           6 ->
               "Saturday"

           _ ->
               "Unknown day"

-}
-- Finding Julian date given Julian day number and time of day


jdateGr : Int -> Int -> Int -> Int -> Int -> Int -> Float
jdateGr y m d hr mn sc =
    let
        jdn : Int
        jdn =
            jdnGr y m d
    in
    toFloat jdn
        + (toFloat hr - 12.0)
        / 24.0
        + toFloat mn
        / 1440.0
        + toFloat sc
        / 86400.0



{-
   The function jdnGr calculates Julian Day Number JDN from the given date
   The arguments: year = y, month = m, day = d
   The function is based to the formula in the wikipedia article "Julian day,
   Converting Gregorian calendar date to Julian Day Number" at
   https://en.wikipedia.org/wiki/
   Julian_day#Converting_Gregorian_calendar_date_to_Julian_Day_Number

   Examples
   > import GregorJDN exposing (jdnGr, jdateGr, weekday, yearLength, isLeapYear)
   > jdnGr 2022 8 16
   2459808 : Int

   The function yearLength is the child of function jdnGr
   Examples
   Year Length 1999
   > yearLength 1999
   365 : Int
   Leap Year 2000 Length
   > yearLength 2000
   366 : Int
   Year 2100
   > yearLength 2100
   365 : Int

   The function isLeapYear result is True if the year length is equal to 366.
   Examples
   > isLeapYear 2000
   True : Bool
   > isLeapYear 2100
   False : Bool
   > isLeapYear 1600
   True : Bool

   Week Days
   > weekday 2022 8 22
   "Monday" : String
   > weekday 2022 8 28
   "Sunday" : String
   6 : Int  Sunday
-}
