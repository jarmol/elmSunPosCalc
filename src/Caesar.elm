module Main exposing (main)

import Html exposing (div, a, text, p, img)
import Html.Attributes exposing (style, href, src, width, alt)

-- Caesarverfahren, Verscieben vom Zeichen ch n Schritte

shiftChar : Char -> Int -> Char
shiftChar ch n =
    Char.fromCode <| 65 + modBy 26 (Char.toCode ch - 65 + n)

-- Wort verschlüsselt

geheim : String -> Int -> String
geheim  klarText aKey = 
   String.map (\n -> shiftChar n aKey) klarText


-- Wir erzeugen den DOM-Baum mit Hilfe von Elm, nicht mit HTML

main = div [style "margin" "5%", style "background-color" "aquamarine"] [
    a [ href "https://de.wikipedia.org/wiki/Caesar-Verschlüsselung" ]
      [ text "Über das Caesar-Verfahren" ]
    , p [] [img [ src "https://kurzelinks.de/caesarbild"
        , width 100
        , alt "Caesar"] []]
    , div [][
      p [] [ text ("HALLOELM key = 3 => " ++ geheim "HALLOELM" 3)]
    , p [] [ text ("HALLOELM key = 6 => " ++ geheim "HALLOELM" 6)]
    , p [] [ text ("HALLOELM key = 8 => " ++ geheim "HALLOELM" 8)]
    , p [] [ text ("HALLOELM key = 9 => " ++ geheim "HALLOELM" 9)]
    , p [] [ text ("HALLOELM key = 13 => " ++ geheim "HALLOELM" 13)]
    ]
    , div [] [
      p [] [ a [href "https://www.inf-schule.de/deklarativ/fp_elm"]
       [text "Funktionale Programmierung mit Elm"]]
    ]
    ]
