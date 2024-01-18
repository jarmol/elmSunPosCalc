module BMIcalc exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type Msg
    = UserTypedHeight String
    | UserTypedWeight String


type alias Model =
    { height : String
    , weight : String
    }


calcBMI mod =
    let
        ht =
            Maybe.withDefault 0.0 (String.toFloat mod.height)

        wt =
            Maybe.withDefault 0.0 (String.toFloat mod.weight)

        fixTo2dec x =
            (toFloat <| round (100 * x)) / 100
    in
    if ht * wt > 0.0 then
        String.fromFloat <| fixTo2dec <| wt / (ht / 100) ^ 2

    else
        "?"


view : Model -> Html Msg
view model =
    layout [ padding 50 ] <|
        column
            [ Border.width 1
            , width fill
            , height fill
            , spacing 30
            ]
            [ header
            , column [ paddingXY 30 10 ]
                [ Input.text [ width <| maximum 300 fill ]
                    { onChange = UserTypedHeight
                    , text = model.height
                    , placeholder = Just <| Input.placeholder [] <| text "Type here"
                    , label = Input.labelAbove [] <| text "Input height [cm]"
                    }
                ]
            , row [ paddingXY 30 10 ]
                [ Input.text [ width <| maximum 300 fill ]
                    { onChange = UserTypedWeight
                    , text = model.weight
                    , placeholder = Just <| Input.placeholder [] <| text "Type here"
                    , label = Input.labelAbove [] <| text "Input weight [kg]"
                    }
                ]
            , row [ paddingXY 30 10 ]
                [ text ("Height " ++ model.height ++ " cm")
                , el [ paddingXY 20 10 ] (text ("Weight " ++ model.weight ++ " kg"))
                ]
            , row [ paddingXY 30 10 ]
                [ myRowOfStuff ("BMI " ++ calcBMI model) ]
            ]


header =
    row
        [ Border.width 1
        , paddingXY 20 10
        , width fill
        ]
        [ text "Polarit"
        , stuff
        ]


stuff =
    el [ alignBottom, centerX ] (text "BMI-Calculator")


myRowOfStuff s =
    row
        [ width fill, centerY, spacing 30 ]
        [ myElement (rgb255 0 120 245) s
        ]


myElement bgcol txt =
    el
        [ Background.color bgcol
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , paddingXY 40 20
        ]
        (Element.text txt)


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserTypedHeight s ->
            { model | height = s }

        UserTypedWeight t ->
            { model | weight = t }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { height = "", weight = "" }
        , view = view
        , update = update
        }

-- Live at https://ellie-app.com/q4HR8m7rhSpa1

