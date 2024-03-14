module RandomLotto exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Svg exposing (circle, svg, text_)
import Svg.Attributes as SA



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
    { randomList : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ -1, -2, -3, -4, -5, -6, -7 ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewList (List Int)



-- generaattorimalli


sevenTo40 : Random.Generator (List Int)
sevenTo40 =
    Random.list 7 (Random.int 1 40)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewList sevenTo40
            )

        NewList newList ->
            ( Model newList, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


listOfBalls =
    [ -1, -2, -3, -4, -5, -6, -7 ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin-left" "15%" ]
        [ h1 [ style "font-family" "Helvetica neue" ] [ text "List of random numbers" ]
        , h2
            [ style "color" "blue"
            , style "font-family" "Helvetica"
            ]
            [ text "( Finnish Lotto )" ]
        , button
            [ style "height" "60px"
            , style "border-radius" "8px"
            , style "font-size" "24px"
            , style "background-color" "gold"
            , onClick Roll
            ]
            [ text "Make a list" ]
        , div
            [ style "font-size" "32px"
            , style "font-family" "Verdana"
            , style "background-color" "#aaffee"
            ]
            [ h2 [] [ Html.text "Lotto" ]
            , makeBalls (takeBallNr 1 model)
            , makeBalls (takeBallNr 2 model)
            , makeBalls (takeBallNr 3 model)
            , makeBalls (takeBallNr 4 model)
            , makeBalls (takeBallNr 5 model)
            , makeBalls (takeBallNr 6 model)
            , makeBalls (takeBallNr 7 model)
            ]
        ]



-- module LottoList exposing (main)
-- elm install elm/svg


takeBallNr : Int -> Model -> Int
takeBallNr n mod =
    if
        (n > 0 && n < 8)
            && not (checkNoSimilar mod 6)
            && not (checkNoSimilar mod 5)
            && not (checkNoSimilar mod 4)
            && not (checkNoSimilar mod 3)
            && not (checkNoSimilar mod 2)
            && not (checkNoSimilar mod 1)
    then
        Maybe.withDefault -1 (List.head (List.drop (n - 1) (List.sort mod.randomList)))

    else
        -1


makeBalls : Int -> Html msg
makeBalls nr =
    svg
        [ SA.viewBox "0 0 200 160"
        , SA.width "200"
        , SA.height "200"
        ]
        [ circle
            [ SA.cx (String.fromInt 140)
            , SA.cy "100"
            , SA.r "40"
            , SA.fill "orange"
            ]
            []
        , text_
            [ SA.x (String.fromInt 140)
            , SA.y "100"
            , SA.fill "black"
            , SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            ]
            [ Svg.text (String.fromInt nr) ]
        ]



-- Check not similar numbers, first 6th to 7th numbers validated


checkNoSimilar : Model -> Int -> Bool
checkNoSimilar mod nr =
    let
        sortedList : List Int
        sortedList =
            List.sort mod.randomList

        ta =
            List.head <| List.drop (nr - 1) sortedList

        tb =
            List.head <| List.drop nr sortedList
    in
    ta == tb



-- https://ckoster22.medium.com/randomness-in-elm-8e977457bf1b
