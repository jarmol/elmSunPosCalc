module Main exposing (..)

-- Input a string of latitude and longitude values.
-- Parse the string to latitude and longitude in degrees and minutes.
-- Use N or S for latitudes and E oe W for longitudes.
-- Note defaults N and E are positive values as S and W are negative.
--   https://guide.elm-lang.org/architecture/forms.html


import Browser
import Html exposing (Html, h2, text, p, div,table,tr,input)
import Html.Attributes exposing (style, placeholder, value, type_)
import Html.Events exposing (onInput)


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { location : String
  , latdeg : Int
  , latmin : Int
  , longdeg : Int
  , longmin : Int
  }


init : Model
init =
  Model "60 10s, 24 42w" 60 10 24 42 


-- UPDATE

type Msg
  = Location String



update : Msg -> Model -> Model
update msg model =
  case msg of
    Location location  ->
      { model | location = location, latdeg = parseLat model }



-- VIEW

parseLat mod =
    mod.latdeg + 2

view : Model -> Html Msg
view model =
  div [style "margin-left" "1cm", style "margin-top" "1cm"]
    [viewInput "text" "input " model.location Location
    , viewResult model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, style "width" "80px", onInput toMsg ] []


viewResult : Model -> Html msg
viewResult model =
   div  [ style "margin-left" "0.5cm"] [
   div  [ style "color" "green" ] [h2 [] [text  "Location latitude & longitude" ]]
   ,table [][
   tr [ style "color" "blue" ] [ text ("Input " ++ model.location )]
   ,tr [ style "color" "blue" ] [ text ("Latitude "
    ++ String.fromInt model.latdeg ++ "° " ++ String.fromInt model.latmin ++ "'")]
   ,tr [ style "color" "blue" ][ text ("Longitude "
   ++  String.fromInt model.longdeg
   ++ "° " ++ String.fromInt model.longmin ++ "'")]]]
