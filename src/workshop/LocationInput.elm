module LocationInput exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import GeoParser exposing (parseLocation)



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
