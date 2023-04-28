module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import TryPureScript (render, withConsole)
import Data.Number (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)


circleArea :: Number -> Number
circleArea r =
  pi * r * r
  

main :: Effect Unit
main = render =<< withConsole do
  log ("sqrt 2.0 = " <>  show (sqrt 2.0))
  log ("Hypotenuse = " <> show (diagonal 3.0 4.0))
  log ("Area of circle with radius 10 = " <> show (circleArea 10.0))
