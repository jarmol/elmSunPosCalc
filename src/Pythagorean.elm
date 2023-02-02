module Pythagorean exposing (main)

import Html exposing (h1, text, div, p)
import Html.Attributes exposing (..)
import String exposing (fromInt)

pythagortriples (m, n) =
   let a = abs (m*m - n*n)
       b = 2*m*n
       c = m*m + n*n
   in  if (a > b) then (b, a, c)
        else (a, b, c)

stringFromTriple (a, b, c) =
   let v = ", "
       sint  = (\n -> String.fromInt n)
       sqint = (\n -> String.fromInt (n * n)) 
   in  "(" ++ (sint a)
       ++ v ++ (sint b) ++ v ++ (sint c) ++ ") -> "
       ++ (sint a) ++ "² + " ++ (sint b) ++ "² = " ++ (sint c) ++ "² -> " 
       ++ (sqint a) ++ " + " ++ (sqint b) ++ " = " ++ (sqint c)

prepare m n =
  "Triple: " ++ ((pythagortriples (m, n)) |> stringFromTriple)


main = div [style "margin-left" "5%"] [
        h1 [style "color" "brown"] [text "Pythagorean triples"]
       ,div [style "margin-left" "5%", style "font-size" "150%", style "color" "blue"] [ 
    p [] [text (prepare 2 1)]
  , p [] [text (prepare 3 2)] 
  , p [] [text (prepare 4 1)] 
  , p [] [text (prepare 4 3)] 
  , p [] [text (prepare 5 2)] 
  , p [] [text (prepare 6 1)] 
  , p [] [text (prepare 5 4)] 
  , p [] [text (prepare 7 2)] 
  , p [] [text (prepare 6 5)] 
  , p [] [text (prepare 8 1)] 
  , p [] [text (prepare 4 7)] 
  , p [] [text (prepare 3 8)] 
  ]]

-- https://ellie-app.com/hdzhNBxmhvja1

