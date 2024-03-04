module Pelikortit exposing (main)

-- https://ckoster22.medium.com/randomness-in-elm-8e977457bf1b

import Browser
import Html exposing (Html, button, div, h1, p, text, a, li, ul)
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


type alias Model =
    { cardSuit : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 0 3)
            )

        NewFace newFace ->
            ( Model newFace
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Kortit
    = Hertta
    | Ruutu
    | Pata
    | Risti


numerostaKortti : Int -> Kortit
numerostaKortti numero =
    case numero of
        0 ->
            Hertta

        1 ->
            Ruutu

        2 ->
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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ style "margin" "10%"
            , style "color" (kartenFarbe (numerostaKortti model.cardSuit))
            ]
            [ text (numerostaKortti model.cardSuit |> kortistaKuva) ]
        , p [ style "height" "80px", style "margin-left" "10%" ]
            [ button
                [ style "height" "80px"
                , style "font-size" "24px"
                , style "background-color" "gold"
                , onClick Roll
                ]
                [ text "Take a card" ]
            ]
        , ul [] [
        li [] [a [href "https://github.com/jarmol/elmSunPosCalc/blob/master/src/Pelikortit.elm"][text "Source: Pelikortit.elm"]]
        , li [] [a [href "https://elm.dmy.fr/packages/elm/random/latest//"][text "Made with package elm / random 1.0.0"]]
        ]]
