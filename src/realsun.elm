port module PortExamples exposing (main)

import Browser
import Html exposing (Html, a, br, button, div, p, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import String exposing (fromFloat, fromInt, toInt)
import Time exposing (..)


geoLocation =
    { latitude = 65.85, longitude = 24.18, timezone = 2.0 }


type alias Model =
    String



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "margin-left" "10%"
        , style "margin-right" "20%"
        , style "font-size" "120%"
        ]
        [ button [ onClick SendDataToJS ]
            [ text "Get date & time from JavaScript!" ]
        , br [] []
        , text ("Posix-time received from JavaScript: " ++ model ++ " ms")
        , br [] []
        , text
            ("Date: "
                ++ (toYearString <| posTime model)
                ++ "-"
                ++ (toMonthString <| posTime model)
                ++ "-"
                ++ (toDayString <| posTime model)
            )
        , br [] []
        , text
            (" Time: "
                ++ zeroTimes (toHourString <| posTime model)
                ++ ":"
                ++ zeroTimes (toMinuteString <| posTime model)
                ++ ":"
                ++ zeroTimes (toSecondString <| posTime model)
            )
        , br [] []
        , text
            (" Julian Day Number (JDN) = "
                ++ fromInt (jdn (year model) (monthNr model) (day model))
            )
        , br [] []
        , text (" Julian Date (JD) = " ++ fromFloat (jdnSecondly model))
        , br [] []
        , text (" Century: " ++ fromFloat (getCentury model))
        , br [] []
        , text (" Right Ascension RA = " ++ fx6 (rectAsc model))
        , br [] []
        , text (" Sun Declination = " ++ fx6 (sunDeclination model) ++ "°")
        , br [] []
        , text (" Solar Zenith     = " ++ fx3 (solZenith model) ++ "°")
        , br [] []
        , text (" HA Sunrise = " ++ fx6 (getHA model))
        , p [ style "color" "red" ]
            [ text
                (" Noon Time "
                    ++ mnToHrMn (getNoon model)
                    ++ " UT + "
                    ++ fromFloat geoLocation.timezone
                    ++ " h ( Longitude "
                    ++ fromFloat geoLocation.longitude
                    ++ " )"
                )
            ]
        , p [ style "color" "red" ]
            [ text
                (" Day Length "
                    ++ mnToHrMn (60.0 * getDayLength model)
                    ++ " ( Latitude "
                    ++ fromFloat geoLocation.latitude
                    ++ " )"
                )
            ]
        , p [ style "color" "red" ] [ text <| riseStr model ]
        , p [ style "color" "red" ] [ text <| sunsetStr model ]
        , p [ style "color" "red" ]
            [ text
                (" Sun Altitude "
                    ++ fx3 (solElev model)
                    ++ "° ( without refraction )"
                )
            ]
        , p [ style "color" "red" ]
            [ text
                (" Sun Altitude "
                    ++ fx3 (refractCorrectAltitude model)
                    ++ "° ( air refraction "
                    ++ fx6 (atmosRefract model)
                    ++ " )"
                )
            ]
        , p [ style "color" "red" ]
            [ text (" Sun Azimuth " ++ fx3 (solAzimuth model) ++ "°") ]
        , viewFooter model
        ]


viewFooter : Model -> Html msg
viewFooter model =
    div [ style "color" "black", style "font-size" "1.0em" ]
        [ p [ style "margin-right" "50%" ]
            [ text
                """
                This programm displays the results calculated
                 in real time for solar positions,
                 Sunrise and Sunset times, for the given
                 geographic location, date and time.
                
                """
            ]
        , p []
            [ a [ href "https://aa.quae.nl/en/reken/zonpositie.html" ] [ text "Astronomical" ]
            , text " calculations"
            ]
        , text "The code is written in "
        , a [ href "https://elm-lang.org/" ] [ text "Elm-language" ]
        , text " version 0.19.1"
        , p [] [ text "Jarmo Lammi © 2020 - 2023" ]
        ]


type Msg
    = SendDataToJS
    | ReceivedDataFromJS Model



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, sendData "Gimme time!" )

        ReceivedDataFromJS data ->
            ( data, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS


port sendData : String -> Cmd msg


port receiveData : (Model -> msg) -> Sub msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )


toYearString msec =
    fromInt (toYear utc (millisToPosix msec))


year : Model -> Int
year mod =
    let
        msec =
            posTime mod
    in
    toYear utc (millisToPosix msec)


monthNr : Model -> Int
monthNr mod =
    let
        msec =
            posTime mod
    in
    Maybe.withDefault 0 (toInt (toMonthString msec))


toDayString msec =
    fromInt (toDay utc (millisToPosix msec))


day : Model -> Int
day mod =
    let
        msec =
            posTime mod
    in
    toDay utc (millisToPosix msec)


toHourString msec =
    fromInt (toHour utc (millisToPosix msec))


hour : Model -> Int
hour mod =
    let
        msec =
            posTime mod
    in
    toHour utc (millisToPosix msec)


jdnHourly mod =
    let
        a =
            jdn (year mod) (monthNr mod) (day mod)
    in
    toFloat a + toFloat (hour mod) / 24.0 - 0.5


toMinuteString msec =
    fromInt (toMinute utc (millisToPosix msec))


minute : Model -> Int
minute mod =
    let
        msec =
            posTime mod
    in
    toMinute utc (millisToPosix msec)


jdnMinutely mod =
    let
        a =
            jdn (year mod) (monthNr mod) (day mod)
    in
    toFloat a + toFloat (hour mod) / 24.0 + toFloat (minute mod) / 1440.0 - 0.5


toSecondString msec =
    fromInt (toSecond utc (millisToPosix msec))


second mod =
    let
        msec =
            posTime mod
    in
    toSecond utc (millisToPosix msec)


jdnSecondly mod =
    let
        a =
            jdn (year mod) (monthNr mod) (day mod)

        sd =
            86400.0
    in
    toFloat a
        + toFloat (hour mod)
        / 24.0
        + toFloat (minute mod)
        / 1440.0
        + toFloat (second mod)
        / sd
        - 0.5


frac : Float -> Float
frac x =
    x - toFloat (floor x)


getCentury : Model -> Float
getCentury mod =
    (jdnSecondly mod - 2451545.0) / 36525.0



{- Not used!
   trueAnomalySun : Model -> Float
   trueAnomalySun mod =
       meanAnomalSun mod + sunEquationOfCenter mod
-}


meanAnomalSun : Model -> Float
meanAnomalSun mod =
    let
        a =
            357.52911

        b =
            35999.05029

        c =
            1.537e-4

        cent =
            getCentury mod
    in
    a + cent * (b + cent * c) |> decNorm360


meanLongitudeSun : Model -> Float
meanLongitudeSun mod =
    let
        cent =
            getCentury mod

        a =
            280.46646

        b =
            36000.76983

        c =
            3.032e-4
    in
    a + cent * (b + cent * c) |> decNorm360


sunTrueLong : Model -> Float
sunTrueLong mod =
    meanLongitudeSun mod + sunEquationOfCenter mod



-- Sun apparent longitude


apparentLongitudeSun : Model -> Float
apparentLongitudeSun mod =
    let
        cent =
            getCentury mod
    in
    sunTrueLong mod - 5.69e-3 - 4.78e-3 * sinDeg (125.04 - 1934.136 * cent)


sunEquationOfCenter : Model -> Float
sunEquationOfCenter mod =
    let
        mAnomalSun =
            meanAnomalSun mod

        cent =
            getCentury mod
    in
    sinDeg mAnomalSun
        * (1.914602 - cent * (0.004817 + 0.000014 * cent))
        + sinDeg (2.0 * mAnomalSun)
        * (0.019993 - 0.000101 * cent)
        + sinDeg (3.0 * mAnomalSun)
        * 0.000289



-- Eccentricy of Earth Orbit, OK tested 11.11.2019


eccentEarthOrbit : Model -> Float
eccentEarthOrbit mod =
    let
        cent =
            getCentury mod

        a =
            0.016708634

        b =
            4.2037e-5

        c =
            1.267e-7
    in
    a - cent * (b + c * cent)



-- Mean Obliquity of Ecliptic


meanObliqEclip : Model -> Float
meanObliqEclip mod =
    let
        cent =
            getCentury mod
    in
    23.0
        + (26.0 + (21.448 - cent * (46.815 + cent * (5.9e-4 - cent * 1.813e-3))) / 60.0)
        / 60.0



-- Corrected obliquity, OK 22.10.19


correctedObliquity : Model -> Float
correctedObliquity mod =
    let
        cent =
            getCentury mod
    in
    meanObliqEclip mod + 0.00256 * cosDeg (125.04 - 1934.136 * cent)


rectAsc mod =
    let
        cent =
            getCentury mod

        oblCorr =
            correctedObliquity mod

        appLongS =
            apparentLongitudeSun mod
    in
    atan2Deg (cosDeg oblCorr * sinDeg appLongS) (cosDeg appLongS)



-- Sun Declination, OK tested 24.10.2019


sunDeclination : Model -> Float
sunDeclination mod =
    let
        cent =
            getCentury mod
    in
    asinDeg (sinDeg (correctedObliquity mod) * sinDeg (apparentLongitudeSun mod))



-- True Solar Time, OK tested 10.02.2020


trueSolTime : Model -> Float
trueSolTime mod =
    let
        hr =
            toFloat (hour mod)

        mn =
            toFloat (minute mod)

        sc =
            toFloat (second mod)

        tz =
            geoLocation.timezone

        e2 =
            60.0 * (hr + tz) + mn + sc / 60.0

        v2 =
            equatTime mod

        b4 =
            geoLocation.longitude
    in
    e2 + v2 + 4.0 * b4 - 60.0 * tz



-- Hour Angle degr. OK tested 17.11.2019


hourAngle mod =
    let
        tSt =
            trueSolTime mod
    in
    if tSt > 0.0 then
        0.25 * tSt - 180.0

    else
        0.25 * tSt + 180.0



-- Solar Zenith (degrees)


solZenith mod =
    let
        b3 =
            geoLocation.latitude

        t2 =
            sunDeclination mod

        hrA =
            hourAngle mod
    in
    acosDeg (sinDeg b3 * sinDeg t2 + cosDeg b3 * cosDeg t2 * cosDeg hrA)



-- Sun altitude without refraction


solElev : Model -> Float
solElev mod =
    90.0 - solZenith mod



-- Atmospheric Refraction


atmosRefract mod =
    let
        elev =
            solElev mod

        cent =
            getCentury mod
    in
    if elev > 85.0 then
        0.0

    else if elev > 5.0 then
        (58.1
            / tanDeg elev
            - 0.07
            / tanDeg elev
            ^ 3
            + 8.6e-5
            / tanDeg elev
            ^ 5
        )
            / 3600.0

    else if elev > -0.575 then
        (1735.0
            + elev
            * (-518.2
                + elev
                * (103.4 + elev * (-12.79 + elev * 0.711))
              )
        )
            / 3600.0

    else
        -20.772 / tanDeg elev / 3600.0


refractCorrectAltitude mod =
    let
        elev =
            solElev mod

        refraction =
            atmosRefract mod
    in
    elev + refraction



--  Solar Azimuth angle clockwise from north, OK tested 19.11.2019


preAzimuth mod =
    let
        b3 =
            geoLocation.latitude

        ad =
            solZenith mod

        t =
            sunDeclination mod
    in
    acosDeg ((sinDeg b3 * cosDeg ad - sinDeg t) / (cosDeg b3 * sinDeg ad))


solAzimuth mod =
    let
        preAz =
            preAzimuth mod

        ac =
            hourAngle mod
    in
    if ac > 0.0 then
        preAz + 180.0 |> decNorm360

    else
        540.0 - preAz |> decNorm360



-- Used in Equation of Time


variableY : Model -> Float
variableY mod =
    let
        cent =
            getCentury mod

        x =
            tanDeg (correctedObliquity mod / 2.0)
    in
    x * x


equatTime mod =
    let
        varY =
            variableY mod

        meanLongS =
            meanLongitudeSun mod

        eOrbitEx =
            eccentEarthOrbit mod

        meanAnomS =
            meanAnomalSun mod
    in
    toDeg
        (varY
            * sinDeg (2.0 * meanLongS)
            - 2.0
            * eOrbitEx
            * sinDeg meanAnomS
            + 4.0
            * eOrbitEx
            * varY
            * sinDeg meanAnomS
            * cosDeg (2.0 * meanLongS)
            - 0.5
            * varY
            * varY
            * sinDeg (4.0 * meanLongS)
            - 1.25
            * eOrbitEx
            * eOrbitEx
            * sinDeg (2.0 * meanAnomS)
        )
        * 4.0



-- Noon time as minutes since midnight


getNoon mod =
    let
        geoLong =
            geoLocation.longitude

        timeZone =
            geoLocation.timezone

        eqTime =
            equatTime mod
    in
    (720.0 - 4.0 * geoLong) - eqTime + timeZone * 60



-- HA of Sunrise hourangle [degrees]


srHA : Model -> Float -> Float
srHA mod zenith =
    let
        geoLat =
            geoLocation.latitude

        declination =
            sunDeclination mod

        x =
            cosDeg zenith
                / (cosDeg geoLat * cosDeg declination)
                - (tanDeg geoLat * tanDeg declination)
    in
    if x > 0.99999 && declination < 0.0 then
        0.0

    else if x < -0.99999 && declination > 0.0 then
        180.0

    else
        acosDeg x


getHA mod =
    srHA mod 90.833



-- Day Length [hours]


getDayLength : Model -> Float
getDayLength mod =
    getHA mod / 7.5



-- Sunrise [mins]


risetMns mod =
    getNoon mod - 4.0 * getHA mod



-- Sunset [mins]


sunsetMns mod =
    getNoon mod + 4.0 * getHA mod


riseStr mod =
    " Sunrise Time      = "
        ++ (mnToHrMn <| risetMns mod)
        ++ " UT + "
        ++ fromFloat geoLocation.timezone
        ++ " h"


sunsetStr mod =
    " Sunset Time       = "
        ++ (mnToHrMn <| sunsetMns mod)
        ++ " UT + "
        ++ fromFloat geoLocation.timezone
        ++ " h"



-- Converts minutes to hh:mn:ss
-- Fixed seconds 25.1.2020


mnToHrMn : Float -> String
mnToHrMn mns =
    let
        lmins =
            remainderBy 60 (floor mns)

        lhrs =
            floor (mns / 60.0)

        lsec =
            floor (60 * mns - toFloat (3600 * lhrs + 60 * lmins))
    in
    zeroFill lhrs
        ++ ":"
        ++ zeroFill lmins
        ++ ":"
        ++ zeroFill lsec



-- Add leading zero if number < 10


zeroFill x =
    if x < 10 then
        "0" ++ String.fromInt x

    else
        String.fromInt x


zeroTimes : String -> String
zeroTimes s =
    if String.length s == 1 then
        "0" ++ s

    else
        s


posTime : String -> Int
posTime posTimeString =
    Maybe.withDefault -1 (String.toInt posTimeString)


toMonthString msec =
    let
        monthName =
            toMonth utc (millisToPosix msec)
    in
    case monthName of
        Jan ->
            "1"

        Feb ->
            "2"

        Mar ->
            "3"

        Apr ->
            "4"

        May ->
            "5"

        Jun ->
            "6"

        Jul ->
            "7"

        Aug ->
            "8"

        Sep ->
            "9"

        Oct ->
            "10"

        Nov ->
            "11"

        _ ->
            "12"



-- Calculate JDN from the current date


jdn y m d =
    (1461 * (y + 4800 + (m - 14) // 12))
        // 4
        + (367 * (m - 2 - 12 * ((m - 14) // 12)))
        // 12
        - (3 * ((y + 4900 + (m - 14) // 12) // 100))
        // 4
        + d
        - 32075


sinDeg alfa =
    sin (toRad alfa)


asinDeg =
    \x -> toDeg (asin x)


cosDeg alfa =
    cos (toRad alfa)


acosDeg =
    \x -> toDeg (acos x)


tanDeg alfa =
    tan (toRad alfa)


atan2Deg =
    \x y -> toDeg (atan2 x y)


toRad =
    \alfa -> pi * alfa / 180.0


toDeg =
    \beta -> 180.0 * beta / pi


decNorm360 =
    \arg -> toFloat (remainderBy 360 (floor arg)) + frac arg


fx3 x =
    round (1.0e3 * x) |> toFloat |> (\t -> t / 1.0e3) |> fromFloat


fx6 x =
    round (1.0e6 * x) |> toFloat |> (\t -> t / 1.0e6) |> fromFloat



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
