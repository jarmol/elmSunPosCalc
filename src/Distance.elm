module Distance exposing (main)

{-| Calculates the distance between the given two locations on Earth.
The locations are defined through the geographic coordinates
latitude and longitude in the unit degree.
<https://ellie-app.com/nwGccLspGGPa1>


# Usage

-- Input the current and destination as location points
-- (latitude, longitude)

-}

import Browser
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), System(..), frenchLocale)
import Html exposing (Html, div, h2, input, table, text, tr)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
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
    format { frenchLocale | decimals = Exact 2, decimalSeparator = "." } (distance mod)


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


viewFooter : Html Msg
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
    let
        radians =
            \v -> v * pi / 180.0

        lat1 =
            radians (getDecVar mod.currLat)

        lat2 =
            radians (getDecVar mod.destLat)

        lon1 =
            radians (getDecVar mod.currLon)

        lon2 =
            radians (getDecVar mod.destLon)
    in
    bearCommon lat2 lat1 lon2 lon1


backBear : Model -> String
backBear mod =
    let
        radians =
            \v -> v * pi / 180.0

        lat2 =
            radians (getDecVar mod.currLat)

        lat1 =
            radians (getDecVar mod.destLat)

        lon2 =
            radians (getDecVar mod.currLon)

        lon1 =
            radians (getDecVar mod.destLon)
    in
    bearCommon lat2 lat1 lon2 lon1


bearCommon : Float -> Float -> Float -> Float -> String
bearCommon fi2 fi1 lm2 lm1 =
    let
        lat2 =
            fi2

        lat1 =
            fi1

        lon2 =
            lm2

        lon1 =
            lm1

        y =
            sin (lon2 - lon1) * cos lat2

        x =
            (cos lat1 * sin lat2) - (sin lat1 * cos lat2 * cos (lon2 - lon1))

        brn =
            atan2 y x

        db =
            if brn < 0 then
                360.0

            else
                0.0
    in
    format { frenchLocale | decimals = Exact 2, decimalSeparator = "." }
        (brn * 180.0 / pi + db)
        ++ "° "
        ++ heading (brn * 180.0 / pi + db)


heading : Float -> String
heading g =
    if (g > 30) && (g < 75) then
        "North-East ↗️"

    else if (g > 75) && (g < 105) then
        "East ➡️"

    else if (g > 105) && (g < 150) then
        "South-East ↘️"

    else if (g > 150) && (g < 210) then
        "South ⬇️"

    else if (g > 210) && (g < 255) then
        "South-West ↙️"

    else if (g > 255) && (g < 285) then
        "West ⬅️"

    else if (g > 285) && (g < 330) then
        "North-West ↖️"

    else if (g > 330) && (g < 360) then
        "North ⬆️"

    else if (g > 0) && (g < 30) then
        "North ⬆️"

    else
        "⁉"


square : Float -> Float
square x =
    x * x
