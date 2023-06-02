module Mathematics.Main where

import Prelude
import Data.Number (sqrt, pi)
import Data.Foldable (fold)
import Effect (Effect)
import Halogen.HTML  as HH
import Halogen as H
import Halogen.Aff.Driver as HA
import Halogen.HTML.Properties as P
import Halogen.HTML.Events as E

type State = Unit

data Query a = NoOp

evalQuery :: forall m. Query ~> H.HalogenM State Query () m
evalQuery = case _ of
  NoOp -> pure

--component :: H.Component HTML Query Unit State
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: evalQuery
  , receiver: const Nothing
  }

main :: Effect Unit
main =
  HA.runUI component unit

render :: State -> H.ComponentHTML Query
render _ =
  HH.div
    [ HH.h1 [ HH.text "Try PureScript!" ]
    , HH.h2_ [ HH.text "Examples" ]
    , HH.p [ HH.text ("Sqrt 2.0 = " <> show (sqrt 2.0)) ]
    , HH.p [ HH.text ("Diagonal 3.0 4.0 = " <> show (diagonal 3.0 4.0)) ]
    , HH.p [ HH.text ("Circumference, radius 5.0 = " <> show (circumference 5.0)) ]
    , HH.p [ HH.text ("Area of circle, radius 10.0 = " <> show (circleArea 10.0)) ]
    , HH.p [ HH.text ("Rectangle border length, width 10.0, height 5.0 = " <> show (rectLength 10.0 5.0)) ]
    , HH.p [ HH.text ("Rectangle area, width 10.0, height 5.0 = " <> show (rectArea 10.0 5.0)) ]
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
    
{- OAI Bot comments
In the modified code, the TryPureScript module is replaced with Halogen.
The render function now uses the Halogen HTML API to create the HTML
elements (h1_, h2_, p_) and the text function to display the text content.
The main function initializes and runs the Halogen component.
The Query and evalQuery definitions are required by Halogen but
since the original code doesn't use any interactive features, the evalQuery function is a no-op.
-}

