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

main:: Effect Unit
main = render =<< withConsole do
  log $  ("TESTING JULIAN DATE NUMBERS 'JDN' AND INFIX OPERATOR '//'")
  log $  ("div 13 2 = "   <> show ( div 13 2))
  log $  ("13 // 2 = "    <> show ( 13 // 2))
  log $  ("-13 // 2 = "   <> show ( -13 // 2))
  log$   ("-12 // 12 = "  <> show ( -12 // 12))
  log$   ("12 // 12 = "   <> show ( 12 // 12))
  log $  ("negdiv 13 2 = "    <> show ( negdiv 13 2))
  log $  ("negdiv (-13) 2 = " <> show ( negdiv (-13) 2))
  log $  ("2023-02-13 JDN = " <> show ( jdnGr 2023 2 13))
  log $  ("2023-02-24 JDN = " <> show ( jdnGr 2023 2 24))
  log $  ("2023-02-28 JDN = " <> show ( jdnGr 2023 2 28))
  log $  ("2023-03-01 JDN = " <> show ( jdnGr 2023 3 1))
  log $  ("2020-02-29 JDN = " <> show ( jdnGr 2020 2 29))
  log $  ("2020-03-01 JDN = " <> show ( jdnGr 2020 3 1))
  
-- testing
-- main = jdnGr 2023 2 13  -- 2459989

