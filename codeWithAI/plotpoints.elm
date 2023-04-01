module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes as SA
import Color exposing (rgb)

-- Plotting the series of points on a line where 
-- the color value red is gradually decreased and
-- color value blue is increased respectively.
--
-- Can you make this code shorter using recursion?

main =
  svg
    [ SA.width "240"
    , SA.height "120"
    , SA.viewBox "0 0 240 120"
    ]
    [ plotPoint  20 60 "rgb(250 190 150)"
    , plotPoint  40 60 "rgb(240 190 160)"
    , plotPoint  60 60 "rgb(230 190 170)"
    , plotPoint  80 60 "rgb(220 190 180)"
    , plotPoint 100 60 "rgb(210 190 190)"
    , plotPoint 120 60 "rgb(200 190 200)"
    , plotPoint 140 60 "rgb(190 190 210)"
    , plotPoint 160 60 "rgb(180 190 220)"
    , plotPoint 180 60 "rgb(170 190 230)"
    , plotPoint 200 60 "rgb(160 190 240)"
    , plotPoint 220 60 "rgb(150 190 250)"
    ]


plotPoint x y color =
      let px = String.fromInt x
          py = String.fromInt y
      in  
          circle
            [ SA.cx px
            , SA.cy py
            , SA.r "10"
            , SA.fill color
            ]   
            []
 
