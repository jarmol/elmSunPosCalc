module Distance exposing (main)

{-| Calculates the distance between the given two locations on Earth.
The locations are defined through the geographic coordinates 
latitude and longitude in the unit degree.

# Usage
-- Input the current and destination as location points
-- (latitude, longitude)
-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { currLat : String
    , currLon : String
    , destLat : String
    , destLon : String
    }


init : Model
init =
    Model "60.17" "24.95" "40.72" "-74.02"



-- UPDATE


type Msg
    = CurrLat String
    | CurrLon String
    | DestLat String
    | DestLon String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CurrLat currLat ->
            { model | currLat = currLat }

        CurrLon currLon ->
            { model | currLon = currLon }

        DestLat destLat ->
            { model | destLat = destLat }

        DestLon destLon ->
            { model | destLon = destLon }



-- VIEW

getDecVar : String -> Float
getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


calcDist : Model -> String
calcDist mod =
    let
        decFix2 =
            \x -> (100 * (x + 0.005) |> floor |> toFloat) / 100.0
    in
    String.fromFloat <| decFix2 (distance mod)


view : Model -> Html Msg
view model =
    div
        [ style "height" "400px"
        , style "background" "linear-gradient(to bottom, #ccccff 0%, #ffffff 100%)"
        , style "margin" "2cm"
        ]
        [ viewInput "text" "Start latit." model.currLat CurrLat
        , viewInput "text" "Start longit." model.currLon CurrLon
        , viewInput "text" "Dest latit." model.destLat DestLat
        , viewInput "text" "Dest longit." model.destLon DestLon
        , viewResult model
        , viewFooter
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, style "width" "80px", onInput toMsg ] []


viewResult : Model -> Html msg
viewResult model =
    div [ style "margin-left" "0.5cm" ]
        [ div [ style "color" "green" ] [ h2 [] [ text "Distance Calculator" ] ]
        , table [ style "color" "darkCyan", style "font-size" "120%" ]
            [ tr [] [ text ("Start Latitude " ++ model.currLat ++ "°") ]
            , tr [] [ text ("Start Longitude " ++ model.currLon ++ "°") ]
            , tr [ style "color" "blue" ] [ text ("End Latitude " ++ model.destLat ++ "°") ]
            , tr [ style "color" "blue" ] [ text ("End Longitude " ++ model.destLon ++ "°") ]
            , tr [] [ text ("Distance " ++ calcDist model ++ " km") ]
            , tr [] [ text ("Bearing " ++ bearing model) ]
            , tr [] [ text ("Back Bearing " ++ backBear model) ]
            ]
        ]



-- Footer text

viewFooter =
    div
        [ style "margin-left" "0.5cm"
        , style "margin-top" "5em"
        ]
        [ text "© Jarmo Lammi 2023"
        ]



distance : Model -> Float
distance mod =
    12742
        -- The diameter of the Earth (KM).
        * asin
            (sqrt
                (square
                    (sin
                        ((getDecVar mod.currLat - getDecVar mod.destLat)
                            * pi
                            / 360
                        )
                    )
                    + cos
                        (getDecVar mod.currLat * pi / 180)
                    * cos (getDecVar mod.destLat * pi / 180)
                    * square
                        (sin
                            ((getDecVar mod.currLon - getDecVar mod.destLon)
                                * pi
                                / 360
                            )
                        )
                )
            )

--- bearing mod

bearing : Model -> String
bearing mod =
    let radians = \v -> v * pi / 180.0
        φ1 = radians(getDecVar mod.currLat )    
        φ2 = radians(getDecVar mod.destLat )
        λ1 = radians(getDecVar mod.currLon )
        λ2 = radians(getDecVar mod.destLon )
    in  bearCommon φ2 φ1 λ2 λ1


backBear : Model -> String
backBear mod =
    let radians = \v -> v * pi / 180.0
        φ2 = radians(getDecVar mod.currLat )    
        φ1 = radians(getDecVar mod.destLat )
        λ2 = radians(getDecVar mod.currLon )
        λ1 = radians(getDecVar mod.destLon )
    in  bearCommon φ2 φ1 λ2 λ1


bearCommon fi2 fi1 lm2 lm1 =
    let φ2 = fi2
        φ1 = fi1
        λ2 = lm2
        λ1 = lm1
        y = (sin (λ2 - λ1)) * (cos φ2)    
        x = (cos(φ1) * sin(φ2)) - (sin(φ1) * cos(φ2) * cos(λ2-λ1))
        θ = atan2 y x 
        dθ = if θ < 0 then 360.0 else 0.0 
        decFix2  =
            \v -> (100 * (v + 0.005) |> floor |> toFloat) / 100.0
    in  (String.fromFloat <| decFix2 (θ*180.0/pi + dθ)) ++ "° " ++ heading (θ*180.0/pi + dθ)

heading : Float -> String
heading g =
        if (g > 30) && (g < 75) then "North-East ↗️"
        else if (g > 75) && (g < 105) then "East ➡️"
        else if (g > 105) && (g < 150) then "South-East ↘️"
        else if (g > 150) && (g < 210) then "South ⬇️"
        else if (g > 210) && (g < 255) then "South-West ↙️"
        else if (g > 255) && (g < 285) then "West ⬅️"
        else if (g > 285) && (g < 330) then "North-West ↖️"
        else if (g > 330) && (g < 360) then "North ⬆️"
        else if (g > 0) && (g < 30) then "North ⬆️"
        else "⁉"

square : Float -> Float
square x =
    x * x


fx3 x =
    round (1.0e3 * x)
