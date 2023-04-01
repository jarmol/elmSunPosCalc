module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes as SA
import Color exposing (rgb)


main =
  let
    green = List.range 1 11 |> List.map (\n -> 190)
    blue = List.range 15 25 |> List.map (\n -> 10*n)
    red = List.reverse blue
    colors = List.map3 (\r g b -> "rgb(" ++ String.fromInt r ++ " " ++ String.fromInt g ++ " " ++ String.fromInt b ++ ")") red green blue
    points = plotPoints 20 60 colors []
  in
    svg 
      [ SA.width "240"
      , SA.height "120"
      , SA.viewBox "0 0 270 120"
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

