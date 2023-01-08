module Distance exposing (main)
-- Input the current and destination as location points
-- (latitude, longitude)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { currLat : String
  , currLon : String
  , destLat : String
  , destLon : String
  }


init : Model
init =
  Model "60.17" "24.95" "65.85" "24.18"


-- UPDATE

type Msg
  = CurrLat String 
  | CurrLon String
  | DestLat String
  | DestLon String


update : Msg -> Model -> Model
update msg model =
  case msg of
    CurrLat currLat ->
      { model | currLat = currLat }

    CurrLon currLon ->
      { model | currLon = currLon }

    DestLat destLat ->
      { model | destLat = destLat }

    DestLon destLon  ->
       { model | destLon = destLon }



-- VIEW

getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)
    
calcDist mod =
   let 
       decThree = \x -> (1000*(x + 0.0005) |> floor |> toFloat ) / 1000.0
   in
     String.fromFloat <| decThree (distance mod)

     
view : Model -> Html Msg
view model =
  div [style "height" "400px"
    , style "background" "linear-gradient(to bottom, #ccccff 0%, #ffffff 100%)"
    , style "margin" "2cm"]
    [ viewInput "text" "Start latit." model.currLat CurrLat
    , viewInput "text" "Start longit." model.currLon CurrLon
    , viewInput "text" "Dest latit." model.destLat DestLat
    , viewInput "text" "Dest longit." model.destLon DestLon
    , viewResult model
    , viewFooter
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, style "width" "80px", onInput toMsg ] []


viewResult : Model -> Html msg
viewResult model =
   div  [ style "margin-left" "0.5cm"] [
   div  [ style "color" "green" ] [h2 [] [text  "Distance Calculator" ]]
   ,table [style "color" "darkCyan", style "font-size" "120%"][
    tr [  ] [ text ("Start Latitude " ++ model.currLat ++ "°")]
   ,tr [  ] [ text ("Start Longitude " ++ model.currLon ++ "°")]
   ,tr [ style "color" "blue" ] [ text ("End Latitude " ++ model.destLat ++ "°")]
   ,tr [ style "color" "blue" ] [ text ("End Longitude " ++ model.destLon ++ "°")]
   ,tr [ ][ text ("Distance " ++ (calcDist model) ++ " km")]]]


viewFooter =
   div  [ style "margin-left" "0.5cm"
        , style "margin-top" "5em"] [
           text "© Jarmo Lammi 2023"
          ]


distance : Model -> Float
distance  mod =
    12742 -- The diameter of the Earth (KM).
    * asin (
        sqrt (
            square (
                sin ((( getDecVar mod.currLat) - (getDecVar mod.destLat) )
                    * pi / 360))
                + cos (
                    (getDecVar mod.currLat) * pi / 180
                )
                    * cos ((getDecVar mod.destLat) * pi / 180 )
                    * square (
                        sin (( (getDecVar mod.currLon) - (getDecVar mod.destLon) )
                            * pi / 360))
        )
    )



square : Float -> Float
square x =
  x * x


fx3 x =
    round (1.0e3 * x)
