module RandomFloats exposing (main)

import Browser
import Html exposing (Html, div, h1, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random


-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { diceFloat: Float
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0.0
  , Cmd.none
  )



-- UPDATE

type Msg
  = Probability
  | NewFloat Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Probability ->
      ( model
      , Random.generate NewFloat nullTo1Generator
      )

    NewFloat newFloat ->
      ( Model newFloat
      , Cmd.none
      )


nullTo1Generator : Random.Generator Float
nullTo1Generator = Random.float 0.0 1.0


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [style "margin" "10%", style "color" "blue"]
    [ h1 [] [ text (String.fromFloat model.diceFloat) ]
    , button [ onClick Probability ] [ text "Probability" ]
    ]

