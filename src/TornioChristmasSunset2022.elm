module TornioChristmasSunset2022 exposing (main)

import Html exposing (div, h1, text, p, img)
import Html.Attributes exposing (style, src, width, height)

main =
    div [style "margin-left" "5%", style "font-family" "Helvetica"] [
       h1 [style "color" "orange"] [text "Christmas Day 2022 in Tornio"]
       , p [style "padding" "1 px"] [img [src "Tornio centre Christmas 2022 sunset.png", width 810, height 506 ] []]
       , p [style "padding" "1 px"] [img [src "Tornio Center 13.50 25.12.2022.png", width 810, height 506 ] [] ]
       , p [style "padding" "1 px"] [img [src "Tornio 13.50 25.12.2022.png", width 810, height 506 ] []]
       , p [style "padding" "1 px"] [img [src "Tornio 13.51 25.12.2022.png", width 810, height 506 ] []]
       , div [style "font-family" "Brush Script MT", style "font-size" "2em", style "color" "blue"] [
         p [style "padding-top" "10 px", style "color" "blue"] [text "Merry Christmas"]
       , p [] [text "Hyvää joulua!"]
       , p [] [text "God jul!"]
       , p [] [text "Frohe Weihnachten"]
       , p [style "color" "blue"] [text "Buon Natale"]
       , p [style "color" "blue"] [text "Feliz Navidad"]
       , p [style "padding-top" "30 px", style "color" "blue"] [text "Jarmo"] ]
       , p [style "color" "black"] [text "Web cam photos from the Swedish, neighboring city Haparanda"]
       , p [] [text "25th December, 2022 at sunset time 01:52 pm"]
 ]
