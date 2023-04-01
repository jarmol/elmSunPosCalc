module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes as SA
import Color exposing (rgb)

main =
  let
    colors = List.range 150 250 10 |> List.map (\b -> "rgb(250 190 " ++ String.fromInt b ++ ")")
    points = plotPoints 20 60 colors []
  in
    svg 
      [ SA.width "240"
      , SA.height "120"
      , SA.viewBox "0 0 240 120"
      ] 
      points

plotPoints x y colors acc =
  case colors of
    [] -> acc
    color :: rest ->
      let 
        px = String.fromInt x
        py = String.fromInt y
        point = circle
                  [ SA.cx px
                  , SA.cy py
                  , SA.r "10"
                  , SA.fill color
                  ]
                  []
      in
        plotPoints (x + 20) y rest (point :: acc)

