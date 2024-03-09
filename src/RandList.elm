module RandList exposing (main )


import Browser
import Html exposing (Html, button, div, h1, h2, text, a, p, li, ul)
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
   randomList : List Int
   }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model  [1, 2, 3, 4, 5, 6, 7]
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
               ,Random.generate NewList sevenTo40)
            

        NewList newList ->
            ( Model newList, Cmd.none)   



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW

view : Model -> Html Msg
view  model = div [style "margin-left" "15%"] 
        [ h1 [style "font-family" "Helvetica neue"][ text ("List of random numbers" ) ]
        , h2 [style "color" "blue"
        , style "font-family" "Helvetica"] [text "( Finnish Lotto )"]
        , p [style "font-size" "36px"
        , style "font-style" "italic" ] [text (Debug.toString model.randomList)]
        , button
                [ style "height" "60px"
                , style "border-radius" "8px"
                , style "font-size" "24px"
                , style "background-color" "gold"
                , onClick Roll
                ]
                [ text "Make a list" ]
        , div [][ 
        ul [] [
        li [] [a [href "https://github.com/jarmol/elmSunPosCalc/blob/master/src/Pelikortit.elm"][text "Source: Pelikortit.elm"]]
        , li [] [a [href "https://elm.dmy.fr/packages/elm/random/latest//"][text "Made with package elm / random 1.0.0"]]
        , li [] [a [href "https://guide.elm-lang.org/effects/random.html"][text "Elm guide - Random"]]
        ]]]


-- https://ckoster22.medium.com/randomness-in-elm-8e977457bf1b