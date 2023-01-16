module Pages.BodyMassIndex exposing (Model, Msg, page)

import Html as H
import Html.Attributes as HA
import Html.Events exposing (onInput)
import Page
import Request
import Shared
import UI
import View exposing (View)



-- Input a weight and length. Show calculated BMI.
-- Use: http://localhost:1234/body-mass-index
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html


page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { weightKG : String
    , lengthCM : String
    }


init : Model
init =
    Model "80" "168"



-- UPDATE


type Msg
    = WeightKG String
    | LengthCM String


update : Msg -> Model -> Model
update msg model =
    case msg of
        WeightKG weightKG ->
            { model | weightKG = weightKG }

        LengthCM lengthCM ->
            { model | lengthCM = lengthCM }


type alias View msg =
    { title : String
    , body : List (H.Html msg)
    }


view : Model -> View Msg
view model =
    { title = "BMI"
    , body =
        UI.layout
            [ H.div [ HA.style "margin-left" "1cm", HA.style "margin-top" "1cm" ]
                [ viewInput "text" "Weight (kg)" model.weightKG WeightKG
                , viewInput "text" "Length (cm)" model.lengthCM LengthCM
                , viewResult model
                ]
            ]
    }


viewInput : String -> String -> String -> (String -> msg) -> H.Html msg
viewInput t p v toMsg =
    H.input [ HA.type_ t, HA.placeholder p, HA.value v, HA.style "width" "80px", onInput toMsg ] []


viewResult : Model -> H.Html msg
viewResult model =
    H.div [ HA.style "margin-left" "0.5cm" ]
        [ H.div [ HA.style "color" "green" ] [ H.h2 [] [ H.text "Body Mass-Index" ] ]
        , H.table []
            [ H.tr [ HA.style "color" "blue" ] [ H.text ("Weight " ++ model.weightKG ++ " kg") ]
            , H.tr [ HA.style "color" "blue" ] [ H.text ("Length " ++ model.lengthCM ++ " cm") ]
            , H.tr [ HA.style "font-size" "120%" ] [ H.text ("BMI " ++ calcBMI model) ]
            ]
        ]


getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


calcBMI model =
    let
        wgt =
            getDecVar model.weightKG

        hgt =
            getDecVar model.lengthCM / 100.0

        -- cm -> m
        decThree =
            \x -> (1000 * (x + 0.0005) |> floor |> toFloat) / 1000.0
    in
    String.fromFloat <| decThree (wgt / hgt / hgt)
