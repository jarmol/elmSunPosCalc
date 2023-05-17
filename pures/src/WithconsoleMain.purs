module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import TryPureScript (render, withConsole)
import Data.Number (sqrt, pi, sin, cos)
import Data.Number.Format (toStringWith, fixed, exponential)

diagonal :: Number -> Number -> Number
diagonal = \w h ->  sqrt (w * w + h * h)


circleArea :: Number -> Number
circleArea  = \r -> pi * r * r

cosDeg :: Number -> Number
cosDeg = \d -> cos (pi * d / 180.0)

sinDeg :: Number -> Number
sinDeg = \d -> sin (pi * d / 180.0)

s60 = toStringWith (fixed 4) ( cosDeg 60.0 ) :: String
s30 = toStringWith (fixed 4) ( sinDeg 30.0 ) :: String

main :: Effect Unit
main = render =<< withConsole do
  log $  "NUMBER FORMAT"
  log $  "sqrt 2.0 ≅ "   <> toStringWith (fixed 6) ( sqrt 2.0 )
  log $  "Hypotenuse of rectangled triangle with catheti 3 and 4 = "
    <> show ( diagonal 3.0 4.0 )
  log $  "Area of circle with radius 10 ≅ "
    <> toStringWith (fixed 6) ( circleArea 10.0 )
  log $  "cos 60° = " <> s60
  log $  "sin 30° = " <> s30
  log $  "sin 30° is equal to cos 60°, "
    <> show (cosDeg 60.0 == sinDeg 30.0)
  log $  "Difference of values cos 60° and sin 30 ≅ "
    <> toStringWith (exponential 4) (cosDeg 60.0 - sinDeg 30.0)
  log $ ("Comparing fixed values as strings, claim these are equal is "
    <> show (s60 == s30))

