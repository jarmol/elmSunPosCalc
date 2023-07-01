module CToFToC exposing (main)

import Browser
import Html exposing (Html, h2, div, p, input, text, table, tr)
import Html.Attributes exposing (type_, placeholder, value, style)
import Html.Events exposing (onInput)



-- MAIN

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
  { inputC : String
  , inputF : String
  }


init : Model
init =
  Model "15" "59"


-- UPDATE

type Msg
  = GotC String
  | GotF String

update : Msg -> Model -> Model
update msg model =
  case msg of
    GotC inputC ->
      { model | inputC = inputC }

    GotF inputF ->
      { model | inputF = inputF }


-- VIEW

view : Model -> Html Msg
view model =
  div [style "margin-left" "1cm", style "margin-top" "1cm"]
    [ p [] [ text "Convert Celsius to Fahrenheit" ]
    , viewInput "text" "input C" model.inputC GotC
    , p [] [ text "Convert Fahrenheit to Celsius" ]
    , viewInput "text" "input F" model.inputF GotF
    , viewResult model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg 
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, style "width" "80px", onInput toMsg ] []


viewResult : Model -> Html msg 
viewResult model =
   div  [ style "margin-left" "0.5cm"] [
   div  [ style "color" "green" ] 
   [h2 [] [text  "Temperature Units Conversion" ]]
   ,table [style "font-size" "120%"][ 
   tr [ style "color" "blue" ]
   [ text ("Temperature " ++ maybeDecVar model.inputC
        ++ " °C = " ++ isLegalC model )]
   
  ,tr [ style "color" "blue" ]
  [ text ("Temperature " ++ maybeDecVar model.inputF
       ++ " °F = " ++ isLegalF model )]
 ]]



maybeDecVar : String -> String
maybeDecVar x =
  case String.toFloat x of
     Just number ->
        String.fromFloat number

     Nothing ->
        "???"


isLegalC : Model -> String
isLegalC model =
        if maybeDecVar model.inputC /= "???"
        then calcF model ++ " °F"
        else "??? °F"  


isLegalF : Model -> String
isLegalF model =
        if maybeDecVar model.inputF /= "???"
        then calcC model ++ " °C"
        else "??? °C" 


getDecVar : String -> Float
getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


calcF : Model -> String
calcF model =
   let celsius = getDecVar model.inputC
       decThree = \x -> (1000*(x + 0.0005) |> floor |> toFloat ) / 1000.0
   in  
     String.fromFloat <| decThree (celsius * 1.8 + 32)


calcC : Model -> String
calcC model =
   let fahrenheit = getDecVar model.inputF
       decThree = \x -> (1000*(x + 0.0005) |> floor |> toFloat ) / 1000.0       
   in
     String.fromFloat <| decThree ((fahrenheit - 32) / 1.8 )


-- https://ellie-app.com/ndfZ4YYnjWta1
