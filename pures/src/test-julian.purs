module Julia.Main where

import Prelude
import Data.Number
import Effect (Effect)
import Effect.Console (log)
import TryPureScript (render, withConsole)

infix 7 negdiv as //

-- Function for Julian day number JDN calculated from the given date
-- where y = year, m = month, d = day

jdnGr :: Int -> Int -> Int -> Int 
jdnGr y m d = 
     (1461 * (y + 4800 + (m - 14) // 12)) // 4
        + (367 * (m - 2 - 12 * ((m - 14) // 12))) // 12
        - ((3 * ((y + 4900 + (m - 14) // 12) // 100)) // 4)
        + d - 32075

negdiv :: Int -> Int -> Int
negdiv n  m = if n < 0 then -(div (-n) m)
              else div n m

main:: Effect Unit
main = render =<< withConsole do
  log $  ("div 13 2 = "   <> show ( div 13 2))
  log $  ("1 + div (-13) 2 = " <> show ( 1 + div (-13) 2))
  log $  ("13 // 2 = "        <> show ( 13 // 2))
  log $  ("-13 // 2 = "       <> show ( -13 // 2))
  log$   ("-12 // 12 = "     <> show ( -12 // 12))
  log$   ("12 // 12 = "     <> show ( 12 // 12))
  log $  ("negdiv 13 2 = "    <> show ( negdiv 13 2))
  log $  ("negdiv (-13) 2 = " <> show ( negdiv (-13) 2))
  log $  ("2023-02-13 JDN = " <> show ( jdnGr 2023 2 13))
  log $  ("2023-02-24 JDN = " <> show ( jdnGr 2023 2 24))
  log $  ("2023-02-28 JDN = " <> show ( jdnGr 2023 2 28))
  log $  ("2023-01-3 JDN = " <> show ( jdnGr 2023 3 1))
  
-- testing
-- main = jdnGr 2023 2 13  -- 2459989
-- Ok 2.5.23
