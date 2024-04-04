module NiceLotto exposing (main)

import Browser
import Html exposing (Html, div, h1, p, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
  { lottoNumber: List Int
  }
   


init : () -> (Model, Cmd Msg)
init _ =
  ( Model [],  Cmd.none)



-- UPDATE

type Msg
  = Lotto
  | NewLotto Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      Lotto ->
        ( model
        , Random.generate NewLotto oneToFortyGenerator)
    

      NewLotto newLotto ->
        ( Model (validateNewNr model newLotto)
        , Cmd.none
        )


validateNewNr : Model -> Int -> List Int
validateNewNr mod newNr =
    if (List.length mod.lottoNumber < 7)
       && not (List.member newNr mod.lottoNumber)
    then List.append mod.lottoNumber [newNr]
    else List.sort mod.lottoNumber


niceList : List Int -> String
niceList alist =
  "[ " ++ ( alist |> List.map String.fromInt |> String.join ", " )
       ++ " ]"



oneToFortyGenerator : Random.Generator Int
oneToFortyGenerator = Random.int 1 40


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW
   

view : Model -> Html  Msg
view model =
  div [style "margin" "10%", style "color" "blue"]
    [ h1 [] [ text ("Lotto numbers" ) ]
    , p [style "font-size" "32px"] [text ( niceList model.lottoNumber)]
    , button [ onClick Lotto
    , style "height" "48px", style "width" "180px"
    , style "font-size" "32px"
    , style "border-radius" "20px"] [ text "Lotto" ]
    ]

-- This works, the most simple example of randomness in elm
-- random integers are saved in a list.
-- Accept max 7 different numbers, reject repeats. 
-- https://ellie-app.com/qLN2VTT5bDwa1
--
-- https://ckoster22.medium.com/randomness-in-elm-8e977457bf1b
