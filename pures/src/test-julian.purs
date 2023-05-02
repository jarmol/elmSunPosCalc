module Julia.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import TryPureScript (render, withConsole)

-- Defining infix operator same as in Elm '//'for division of integers

infix 8 negdiv as //

-- Function for Julian day number JDN calculated from the given date
-- where y = year, m = month, d = day

jdnGr :: Int -> Int -> Int -> Int 
jdnGr y m d = 
     (1461 * (y + 4800 + (m - 14) // 12)) // 4
        + (367 * (m - 2 - 12 * (m - 14) // 12)) // 12
        - (3 * ((y + 4900 + (m - 14) // 12) // 100) // 4)
        + d - 32075

-- Defining new function 'negdiv' replacing 'div' to give same results
-- as Elm integer-division '//'also for negative dividend
negdiv :: Int -> Int -> Int
negdiv n  m = if n < 0 then -(div (-n) m)
              else div n m


weekday :: Int -> Int -> Int -> String
weekday y m d =
    let
        dayNumber =
            mod (1 + jdnGr y m d) 7
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
            "undefined"



main :: Effect Unit
main  =
  render =<< withConsole do
  log $  ("TESTING JULIAN DATE NUMBERS 'JDN' AND INFIX OPERATOR '//'")
  log $  ("div 13 2 = "   <> show ( div 13 2 == 6 ))
  log $  ("13 // 2 = "    <> show ( 13 // 2 == 6 ))
  log $  ("-13 // 2 = "   <> show ( -13 // 2 == -6 ))
  log $  ("-12 // 12 = "  <> show ( -12 // 12 == -1 ))
  log $  ("12 // 12 = "   <> show ( 12 // 12 == 1 ))
  log $  ("negdiv 13 2 = "    <> show ( negdiv 13 2 == 6 ))
  log $  ("negdiv (-13) 2 = " <> show ( negdiv (-13) 2 == -6 ))
  log $  (printAB 2023 2 24)
  log $  (printAB 2020 2 28)
  log $  (printAB 2023 3 1)
  log $  (printAB 2020 2 29)
  log $  (printAB 2020 3 1)  
  log $  ("Weekday 2023-04-30 = ") <> ( weekday 2023 4 30 )
  log $  ("Weekday 2023-05-01 = ") <> ( weekday 2023 5 1 )
  log $  ("Weekday 2023-05-02 = ") <> ( weekday 2023 5 2 )

  
printA :: Int -> Int -> Int -> String
printA j m t =
      show j <> "-" <> show m <> "-" <> show t

printB :: Int -> Int -> Int -> String
printB j m t =
      " JDN = " <> show (jdnGr j m t)

printAB ::  Int -> Int -> Int -> String
printAB j m t =
  (printA j m t) <> (printB j m t)

-- testing
-- main = jdnGr 2023 2 13  -- 2459989

