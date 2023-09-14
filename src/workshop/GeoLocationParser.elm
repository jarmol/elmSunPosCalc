module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, float, spaces)
import Html exposing (h1, text, div, p)


type alias GeoLocation =
  { latdeg : Int
  , latmin : Int
  , londeg : Int
  , lonmin : Int
  }

geoLocation : Parser GeoLocation
geoLocation =
  succeed GeoLocation
    |. symbol "("
    |. spaces
    |= int
    |. spaces
    |= int
    |. spaces
    |= int
    |. spaces
    |= int
    |. spaces
    |. symbol ")"


p1 = "( 10 15 S 24 51 W )"


getFlagLat : String -> Int
getFlagLat ipx =
  case String.contains "S" ipx of
      True -> -1
      _    -> 1




getFlagLon : String -> Int
getFlagLon ipx =
  if String.contains "W" ipx
     then -1
     else 1



prepareInput : String -> String
prepareInput ipx =
  String.replace "S" ""
   <| String.replace "N" ""
   <| String.replace "E" ""
   <| String.replace "W" "" ipx


parseLocation ipLoc =
  let ns = getFlagLat ipLoc
      ew = getFlagLon ipLoc
      v1 = prepareInput ipLoc 
      zero : GeoLocation
      zero = {latdeg = 0, latmin = 0, londeg = 0, lonmin = 0}
      pz = Result.withDefault  zero ( Parser.run geoLocation v1 )
      latDecimal = toFloat (ns * ( 60 * pz.latdeg + pz.latmin)) / 60.0
      lonDecimal = toFloat (ew * ( 60 * pz.londeg + pz.lonmin)) / 60.0
   in (latDecimal, lonDecimal)


p2 = "( 60 15 N 24 51 E )"


p3 = "(52 33 N 13 24 E )"

p4 = "(40 42 N 74 24 W )"

result1 = parseLocation p1

result2 = parseLocation p2

result3 = parseLocation p3

result4 = parseLocation p4

viewResult rx result =
  div [] [
   h1 [] [text "Geolocation parser"]
   ,p [] [ text ("Parsing input string " ++ rx ++ " => ")]
   ,p [] [ text ("Latitude = "
   ++ String.fromFloat (Tuple.first result) ++ "°"
   ++ " Longitude = " ++ String.fromFloat (Tuple.second result) ++ "°")]
   ]
   
main = 
  div [] [viewResult p1 result1
  , div [] [viewResult p2 result2]
  , div [] [viewResult p3 result3]
  , div [] [viewResult p4 result4]
  ]
-- https://ellie-app.com/nTGyb8bd6pra1
