module PrimeFactorizer exposing (main)

import Browser
import Html exposing (Html, div, h1, input, span, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)



-- MAIN


accumulator : List Int
accumulator =
    []


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { cnumber : String
    }


init : Model
init =
    { cnumber = "84894624407" }



-- UPDATE


type Msg
    = CNumber String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CNumber cnumber ->
            { model | cnumber = cnumber }



-- VIEW


compositeNr : Model -> Int
compositeNr mod =
    Maybe.withDefault 0 (String.toInt mod.cnumber)


ts : Model -> List Int
ts mod =
    showFactors (compositeNr mod) 2 accumulator


view : Model -> Html Msg
view model =
    div [ style "margin-left" "10%", style "margin-right" "20%" ]
        [ h1 [] [ text "Prime Factorizer" ]
        , span [ style "background-color" "blue", style "color" "white" ]
            [ text "Number "
            , viewInput "number" "Give number" model.cnumber CNumber
            ]
        , div
            [ style "margin" "5%"
            , style "font-size" "1.5em"
            , style "color" "blue"
            ]
            [ text
                ("Prime factors: "
                    ++ listAsString (ts model)
                    ++ " from number "
                    ++ String.fromInt (List.product (ts model))
                )
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, style "width" "120px", onInput toMsg ] []


showFactors : Int -> Int -> List Int -> List Int
showFactors number factor acc =
    if number < 2 then
        acc
        -- returns the final result if number < 2

    else if
        modBy factor number == 0
        -- modulo used to get prime factors
    then
        let
            v2 : List Int
            v2 =
                factor :: acc

            number2 : Int
            number2 =
                number // factor
        in
        showFactors number2 factor v2
        -- recursive call
        -- this modulus function is used
        -- in order to output factor !=2

    else
        let
            factor2 : Int
            factor2 =
                factor + 1
        in
        showFactors number factor2 acc


listAsString : List Int -> String
listAsString myList =
    List.map String.fromInt myList
        |> List.map (\el -> " " ++ el)
        |> List.foldl (++) " "



-- Driver program to test above function
-- compositeNr = 84894624407
--      result = 3067 4357 6353
-- https://ellie-app.com/pMsTfhVCmTma1
