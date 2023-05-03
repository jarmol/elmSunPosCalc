module Mathematics.Main where

import Prelude
import Data.Number (sqrt, pi)
import Data.Foldable (fold)
import Effect (Effect)
import TryPureScript (h1, h2, p, text, render)

main :: Effect Unit
main =
  render $ fold
    [ h1 (text "Try PureScript!")
    , h2 (text "Examples")
    , p (text ("Sqrt 2.0 = " <> show (sqrt 2.0)))
    , p (text ("Diagonal 3.0 4.0 = " <> show (diagonal 3.0 4.0)))
    , p (text ("Circumference, radius   5.0 = " <> show (circumference 5.0)))
    , p (text ("Area of circle, radius 10.0 = " <> show (circleArea 10.0)))
    , p (text ("Rectangle border length, width 10.0, height 5.0 = " <> show (rectLength 10.0 5.0)))
    , p (text ("Rectangle area, width 10.0, height 5.0 = " <> show (rectArea 10.0 5.0)))
   ]
  where
  
  diagonal :: Number -> Number -> Number
  diagonal = \x y -> sqrt (x*x + y*y)
 
  circumference :: Number -> Number
  circumference = \r -> 2.0*pi*r
  
  circleArea :: Number -> Number
  circleArea = \r -> pi*r*r
  
  rectArea :: Number -> Number -> Number
  rectArea = \wide high -> wide * high
  
  rectLength :: Number -> Number -> Number
  rectLength = \wide high -> 2.0 * (wide + high)

