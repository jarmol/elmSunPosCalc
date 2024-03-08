module Pelikortit exposing (main)


import Browser
import Html exposing (Html, button, div, h1, h2, p, text, a, li, ul)
import Html.Attributes exposing (style, href)
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
 

type alias Model = {
   randomPoint : ( Int, Int )
   }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model  (1, 1)
    , Cmd.none
    )


-- UPDATE

type Msg
    = Roll
    | NewPoint (Int,Int)

randomPoint : Random.Generator ( Int, Int )
randomPoint =
    Random.pair (Random.int 1 4) (Random.int 1 13)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
               ,Random.generate NewPoint randomPoint)
            

        NewPoint newPoint ->
            ( Model newPoint, Cmd.none)   



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Kortit
    = Hertta
    | Ruutu
    | Pata
    | Risti


numerostaKortti : Model -> Kortit
numerostaKortti mod =
    case (Tuple.first mod.randomPoint) of
        1 ->
            Hertta

        2 ->
            Ruutu

        3 ->
            Pata

        _ ->
            Risti


kortistaKuva : Kortit -> String
kortistaKuva kortti =
    case kortti of
        Hertta ->
            "Heart ♥"

        Ruutu ->
            "Diamond ♦"

        Pata ->
            "Spade ♠"

        _ ->
            "Club ♣"


kartenFarbe : Kortit -> String
kartenFarbe karte =
    case karte of
        Hertta ->
            "red"

        Ruutu ->
            "red"

        _ ->
            "black"


kortinNumero : { a | randomPoint : ( b, Int ) } -> String
kortinNumero mod =
    case (Tuple.second mod.randomPoint) of
        1 ->
             "A"

        11 ->
              "J"

        12 ->
              "Q"

        13 ->
              "K"
       
        _  ->
              String.fromInt (Tuple.second mod.randomPoint)

-- VIEW

view : Model -> Html Msg
view model = 
    div []
        [ h1
            [ style "margin-left" "10%"
            , style "color" (kartenFarbe (numerostaKortti model))
            ]
            [ text (numerostaKortti model |> kortistaKuva ) ]
        , h2 [style "margin-left" "10%"]
            [ text ("No: " ++ kortinNumero model) ]
        , p [ style "height" "80px", style "margin-left" "10%" ]
            [ button
                [ style "height" "60px"
                , style "border-radius" "8px"
                , style "font-size" "24px"
                , style "background-color" "gold"
                , onClick Roll
                ]
                [ text "Take a card" ]
            ]
        , ul [] [
        li [] [a [href "https://github.com/jarmol/elmSunPosCalc/blob/master/src/Pelikortit.elm"][text "Source: Pelikortit.elm"]]
        , li [] [a [href "https://elm.dmy.fr/packages/elm/random/latest//"][text "Made with package elm / random 1.0.0"]]
        , li [] [a [href "https://guide.elm-lang.org/effects/random.html"][text "Elm guide - Random"]]
        ]]


-- https://ckoster22.medium.com/randomness-in-elm-8e977457bf1b
