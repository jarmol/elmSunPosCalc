module LocationParser exposing (main)

import Html
import Debug

-- s = "65 15s 20 12w"
--s="65 15n 20 12w"
--s = "65 15n 20 12e"
s = "65 15 20 12"

u = String.toUpper s
south = String.contains "S" u
west = String.contains "W" u
ns = if south then "S" else "N"
ew = if west then "W" else "E"
nrs = String.replace "S" ""
   <| String.replace "N" ""
   <| String.replace "W" ""
   <| String.replace "E" "" u
   
nrl = String.split " " nrs
lsmaybeint = List.map String.toInt nrl
lsisint = List.map intFromJust lsmaybeint
latdeg = intFromJust <| List.head lsisint
latmin = intFromJust <| List.head <| List.drop 1 lsisint
latdecim1 = toFloat latdeg + (toFloat latmin) / 60.0
latdecim  = if south then -1.0 * latdecim1 else latdecim1
londeg = intFromJust <| List.head <| List.drop 2 lsisint
lonmin = intFromJust <| List.head <| List.drop 3 lsisint
londecim1 = toFloat londeg + (toFloat lonmin) / 60.0
londecim  = if west then -1.0 * londecim1 else londecim1

intFromJust justInt =
   case justInt of
       Just n ->
           n
       _      ->
           0

t2 = String.toUpper "asdf"

main = Html.div [] [
       Html.text (u ++ " => " ++ ns ++ ", " ++ ew)
       , Html.p [] [Html.text nrs]
       , Html.p [] [Html.text (Debug.toString nrl)]
       , Html.p [] [Html.text (Debug.toString lsmaybeint)]
       , Html.p [] [Html.text (Debug.toString lsisint)]
       , Html.p [] [Html.text ("Latitude "
       ++ String.fromInt latdeg ++ "° ")
       , Html.text (String.fromInt latmin ++ "' " ++ ns)
       , Html.text (" Longitude "
       ++ String.fromInt londeg ++ "° ")
       , Html.text (String.fromInt lonmin ++ "' " ++ ew)]
       , Html.p [] [Html.text ("Latitude decim. = "
       ++ String.fromFloat latdecim ++ "° ")
       , Html.text (" Longitude decim. = "
       ++ String.fromFloat londecim ++ "° ")]
       ]
