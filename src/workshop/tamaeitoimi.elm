module LocationInput exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, float, spaces)
-- import GeoParser exposing (parseLocation)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "60 15 N 24 51 E" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW

getLocation inputS =
  let r2 = parseLocation inputS
  in  "Latitude " ++ String.fromFloat (Tuple.first r2)
      ++ " Longitude " ++ String.fromFloat (Tuple.second r2)


view : Model -> Html Msg
view model =
  div [style "margin" "5%"]
    [ input [ placeholder "Text to parse", value model.content, onInput Change ] []
    , div [] [ text (getLocation model.content) ]
    ]


-- module GeoParser exposing (parseLocation)

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
