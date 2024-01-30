module FatPlusBMI exposing (main)

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
    | UserTypedAge String
    | UserChose UserGender

type UserGender
    = Female
    | Male


type alias Model =
    { height : String
    , weight : String
    , age : String
    , gender : UserGender
    }


calcBMI mod =
    let
        ht =
            Maybe.withDefault 0.0 (String.toFloat mod.height)

        wt =
            Maybe.withDefault 0.0 (String.toFloat mod.weight)

    in  if ht * wt > 0.0 then
       wt / (ht / 100) ^ 2

    else
        0.0


calcFat mod =
    let bmi = calcBMI mod
        genderVal = if mod.gender == Female
                    then 1.0
                    else 0.0
        age  = Maybe.withDefault 0.0 (String.toFloat mod.age)
    in 
        0.503*age + 10.689*genderVal + 3.172*bmi
        - 0.026*bmi^2 + 0.181*bmi*genderVal - 0.02*bmi*age
        - (0.005*bmi^2)*genderVal + (0.00021*bmi^2)*age - 44.988


finishVal x =
    let fixTo2val =
           (toFloat <| round (100 * x)) / 100
    in 
        if fixTo2val > 0
        then String.fromFloat fixTo2val
        else "?"

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
            [ row [] 
                [ Input.text [ width <| maximum 300 fill ]
                    { onChange = UserTypedHeight
                    , text = model.height
                    , placeholder = Just <| Input.placeholder [] <| text "Type here"
                    , label = Input.labelAbove [] <| text "Input height [cm]"
                    }
            , el [ paddingXY 30 5]
                 (Input.radio
                    [ padding 10, spacing 20]
                    { onChange = UserChose 
                    , selected = Just model.gender
                    , label = Input.labelAbove [] <| text "Gender selection"
                    , options =
                        [ Input.option Male <| text "M"
                        , Input.option Female <| text "F"
                        ]
                    }
                 )
                ]
           , row [spacing 20 ]
                [ Input.text [ width <| maximum 300 fill ]
                    {  onChange = UserTypedWeight
                    , text = model.weight
                    , placeholder = Just <| Input.placeholder [] <| text "Type here"
                    , label = Input.labelAbove [] <| text "Input weight [kg]"
                    }
                   --   , Seuraavaksi  age-tieto
           ,     Input.text [ width <| maximum 300 fill ]
                    {  onChange = UserTypedAge
                    , text = model.age
                    , placeholder = Just <| Input.placeholder [] <| text "Type here"
                    , label = Input.labelAbove [] <| text "Input age [years]"
                    } 
                ]]

            , row [ paddingXY 30 10 ]
                [ text ("Height " ++ model.height ++ " cm")
                , el [ paddingXY 20 10 ] (text ("Weight " ++ model.weight ++ " kg"))
                ]
            , row [ paddingXY 30 10 ]
                [ myRowOfStuff (rgb255 0 120 245) ("BMI " ++ (finishVal <| calcBMI model))
                , myRowOfStuff (rgb255 245 0 245) ("FAT% " ++ ( finishVal <| calcFat model))
                ]

            ]

optionGender sel =
        if sel == Male then "male"
        else if sel == Female then "female"
        else "Gender is undefined!"

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
    el [ alignBottom, centerX ] (text "BMI- and Body Fat-Calculator")


myRowOfStuff col s =
    row
        [ width fill, centerY, spacing 30 ]
        [ myElement col s
        ]


myElement bgcol txt =
    el
        [ Background.color bgcol
        , Font.color (rgb255 255 255 255)
        , Border.rounded 6
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

        UserTypedAge y ->
            { model | age = y }

        UserChose g ->
            { model | gender = g }

main : Program () Model Msg
main =
    Browser.sandbox
        { init = { height = "", weight = "", age = "", gender = Male}
        , view = view
        , update = update
        }

-- Live at https://ellie-app.com/q4HR8m7rhSpa1

