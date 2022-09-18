module Solar exposing (..)

import Html exposing (div, h1, text, p, Attribute)
import Html.Attributes exposing (..)
import Main exposing (Model,getCent,getNoon,sunRise,sunSet,getDayLength,mnToHrMn,sunDeclination)

gw = Model "2022" "9" "23" "1" "4" "27" "51.4825766"  "-0.0076589" "0"
gwDate = gw.year ++ "-" ++ gw.month ++ "-" ++ gw.day
preZero = if (String.length gw.minute == 1) then "0" else ""
gwTime = gw.hour ++ ":" ++ preZero ++ gw.minute ++ ":" ++ gw.second
gwNoon = mnToHrMn <| getNoon gw
gwAutumnEquinoxDeclination = sunDeclination gw

gwt = Model "2022" "9" "23" "11" "52" "33" "50"  "0.00" "0"
gwAltNoon = 90 - Main.solZenith gwt
gwAzimNoon = Main.solAzimuth gwt

main = div [style "margin" "2em"] 
  [h1 [] [text "Autumn Equinox"]
   , text ("Latitude " ++ gwt.latitude ++ "° Longitude " ++ gwt.longitude
      ++ "°"  ) 
   ,p [] [text ("Autumn Equinox Date " ++ gwDate)]
   ,p [] [text ("Autumn Equinox Time GMT " ++ gwTime)]  
   ,p [] [text ("Autumn Equinox Solar Declination: "
     ++ String.fromFloat gwAutumnEquinoxDeclination ++ "°"
     ++ " ( expected 0.000° )")]
   ,p [] [text ("Solar Noon Time: GMT " ++ gwNoon)]
   ,p [] [text ("Sun Altitude at Noon: "
      ++ String.fromFloat gwAltNoon ++ "°" ++ " ( expected max 40° )")]
   ,p [] [text ("Solar Azimuth at Noon: "
      ++ String.fromFloat gwAzimNoon ++ "°"
      ++ " ( expected 180.000° )")]
  ]
