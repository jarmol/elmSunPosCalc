module Lotto2 exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, text)
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


tenTo40 : Random.Generator (List Int)
tenTo40 =
    Random.list 10 (Random.int 1 40)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewList tenTo40
            )

        NewList newList ->
            ( Model newList, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
            [ h2 [] [ Html.text "Lotto v0.2" ]
            , makeBalls (takeBallNr 1 model)
            , makeBalls (takeBallNr 2 model)
            , makeBalls (takeBallNr 3 model)
            , makeBalls (takeBallNr 4 model)
            , makeBalls (takeBallNr 5 model)
            , makeBalls (takeBallNr 6 model)
            , makeBalls (takeBallNr 7 model)
            ]
        ]



-- The final list shall consist of seven different numbers
-- Collect the final numbers  from the raw list


pairsRemoved : List Int -> ( Int, List Int )
pairsRemoved randInt =
    let
        nr1 : Int
        nr1 =
            Maybe.withDefault -1 (List.head randInt)

        ran2 : List Int
        ran2 =
            List.filter (\n -> n /= nr1) randInt
    in
    ( nr1, ran2 )


recursive : List Int -> List Int -> List Int
recursive work accum =
    let
        tupleA : ( Int, List Int )
        tupleA =
            pairsRemoved work

        accum2 : List Int
        accum2 =
            Tuple.first tupleA :: accum

        work2 : List Int
        work2 =
            Tuple.second tupleA
    in
    if List.length accum < 7 then
        recursive work2 accum2

    else
        accum


takeBallNr : Int -> Model -> Int
takeBallNr n mod =
    if n > 0 && n < 8 then
        Maybe.withDefault -1 (List.head (List.drop (n - 1) <| recursive mod.randomList []))

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



-- https://suncalc.lammi.cc/calculus/lotto.html
