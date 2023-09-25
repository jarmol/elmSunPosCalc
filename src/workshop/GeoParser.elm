module GeoParser exposing (parseLocation)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, float, spaces)
-- import Html exposing (h1, text, div, p)


type alias GeoLocation =
  { latdeg : Int
  , latmin : Int
  , londeg : Int
  , lonmin : Int
  }

geoLocation : Parser GeoLocation
geoLocation =
  succeed GeoLocation
    |= int
    |. spaces
    |= int
    |. spaces
    |= int
    |. spaces
    |= int


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

-- https://ellie-app.com/nTGyb8bd6pra1
