module Euler where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.List (range,filter,length)
import Data.Foldable (sum)
-- import TryPureScript (render, withConsole)

a = range 3 999


multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0 ) a

totalMultiples :: Int
totalMultiples = length multiples

main :: Effect Unit
main =  do
  log $ "Number of 3, 5 multiples = " <> show totalMultiples
  log $ "Sum of multiples = " <> show (sum multiples)
