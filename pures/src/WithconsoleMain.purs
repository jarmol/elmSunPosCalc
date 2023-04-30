module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import TryPureScript (render, withConsole)
import Data.Number (sqrt, pi, sin, cos, round)

diagonal :: Number -> Number -> Number
diagonal = \w h ->  sqrt (w * w + h * h)


circleArea :: Number -> Number
circleArea  = \r -> pi * r * r

cosDeg :: Number -> Number
cosDeg = \d -> cos (pi * d / 180.0)

sinDeg :: Number -> Number
sinDeg = \d -> sin (pi * d / 180.0)

fixTo6Decim :: Number -> Number
fixTo6Decim x = 1.0e-7 * round (1.0e7*x)

main :: Effect Unit
main = render =<< withConsole do
  log $  ("sqrt 2.0 = "   <> show ( sqrt 2.0 ))
  log $  ("Hypotenuse = " <> show ( diagonal 3.0 4.0 ))
  log $  ("Area of circle with radius 10 = "
  <> show ( circleArea 10.0 ))
  log $  ("cos 60° = " <> show ( cosDeg 60.0 ))
  log $  ("sin 30° = " <> show ( sinDeg 30.0 ))
  log $  ("sin 30° is equal to cos 60°, "
  <> show (cosDeg 60.0 == sinDeg 30.0))
  log $  ("Difference cos 60° - sin 30 = "
  <> show (cosDeg 60.0 - sinDeg 30.0))
  log $  ("sin 30° rounded = "
  <> show (fixTo6Decim (sinDeg 30.0)))
  log $  ("cos 60° rounded = "
  <> show (fixTo6Decim (cosDeg 60.0)))
  log $  ("Rounded sin 30° is equal to rounded cos 60°, "
  <> show (fixTo6Decim (cosDeg 60.0) == fixTo6Decim (sinDeg 30.0)))
