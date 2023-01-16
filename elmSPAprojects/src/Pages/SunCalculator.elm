module Pages.SunCalculator exposing (Model, Msg, page)

import Gen.Params.SunCalculator exposing (Params)
import Html as H exposing (Html, div, p, text)
import Html.Attributes as HA
import Html.Events exposing (onInput)
import Page
import Request
import Shared
import String exposing (fromFloat, fromInt, toInt)
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { year : String
    , month : String
    , day : String
    , hour : String
    , minute : String
    , second : String
    , latitude : String
    , longitude : String
    , timezone : String
    }


init : Model
init =
    Model "2022" "12" "7" "10" "14" "40" "65.85" "24.18" "2"



-- UPDATE


type Msg
    = Year String
    | Month String
    | Daynumber String
    | Hour String
    | Minute String
    | Second String
    | Latitude String
    | Longitude String
    | Timezone String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Year year ->
            { model | year = year }

        Month month ->
            { model | month = month }

        Daynumber day ->
            { model | day = day }

        Hour hour ->
            { model | hour = hour }

        Minute minute ->
            { model | minute = minute }

        Second second ->
            { model | second = second }

        Latitude latitude ->
            { model | latitude = latitude }

        Longitude longitude ->
            { model | longitude = longitude }

        Timezone timezone ->
            { model | timezone = timezone }


sunEqCntr : Float -> Float
sunEqCntr cent =
    sinDeg (meanAnomalSun cent)
        * (1.914602 - cent * (0.004817 + 0.000014 * cent))
        + sinDeg (2.0 * meanAnomalSun cent)
        * (0.019993 - 0.000101 * cent)
        + sinDeg (3.0 * meanAnomalSun cent)
        * 0.000289



-- Eccentricy of Earth Orbit, OK tested 11.11.2019


eccentEarthOrbit : Float -> Float
eccentEarthOrbit cent =
    0.016708634
        - cent
        * (4.2037e-5 + 1.267e-7 * cent)



-- Mean anomality of Sun, OK tested 10.11.2019


meanAnomalSun : Float -> Float
meanAnomalSun cent =
    let
        a =
            357.52911

        b =
            35999.05029

        c =
            1.537e-4
    in
    a + cent * (b + cent * c) |> decNorm360


getCentury : Model -> Float
getCentury mod =
    postCent (getJDN mod) (fJD mod)



-- Mean Longitude of Sun, OK tested 10.11.2019


calcSunML : Float -> Float
calcSunML cent =
    let
        a =
            280.46646

        b =
            36000.76983

        c =
            3.032e-4
    in
    a + cent * (b + cent * c) |> decNorm360



-- Sun true longitude, OK tested 12.11.2019


trueLongSun : Model -> Float
trueLongSun mod =
    let
        cent =
            getCentury mod
    in
    calcSunML cent + sunEqCntr cent



-- Sun apparent longitude, OK tested 22.10.19


appLongSun : Model -> Float
appLongSun mod =
    trueLongSun mod - 5.69e-3 - 4.78e-3 * sinDeg (125.04 - 1934.136 * getCentury mod)



-- Mean Obliquity of Ecliptic


meanObliqEclip : Model -> Float
meanObliqEclip mod =
    let
        cent =
            getCentury mod
    in
    23.0 + (26.0 + (21.448 - cent * (46.815 + cent * (5.9e-4 - cent * 1.813e-3))) / 60.0) / 60.0



-- Corrected obliquity, OK 22.10.19


obliqCorr : Model -> Float
obliqCorr mod =
    meanObliqEclip mod + 0.00256 * cosDeg (125.04 - 1934.136 * getCentury mod)



-- Right Ascension RA, OK tested 14.11.2019


rectAsc : Model -> Float
rectAsc mod =
    let
        appLongS =
            appLongSun mod
    in
    atan2Deg (cosDeg (obliqCorr mod) * sinDeg appLongS) (cosDeg appLongS)



-- Used in Equation of Time


variableY : Model -> Float
variableY mod =
    let
        x =
            tanDeg (obliqCorr mod / 2.0)
    in
    x * x



-- Equation of Time, OK tested 15.11.2019


equatTime : Model -> Float
equatTime mod =
    let
        cent =
            getCentury mod

        varY =
            variableY mod

        meanLongS =
            calcSunML cent

        eOrbitEx =
            eccentEarthOrbit cent

        meanAnomS =
            meanAnomalSun cent
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



-- HA of Sunrise hourangle, OK tested 15.11.2019


srHA mod zenith =
    let
        geoLat =
            getDecVar mod.latitude

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



-- Civil twilight Sunrise HA


getCivTwHA mod =
    srHA mod 96.0



-- Noon time as minutes since midnight


getNoon mod =
    let
        geoLong =
            getDecVar mod.longitude

        timeZone =
            getDecVar mod.timezone

        eqTime =
            equatTime mod
    in
    (720.0 - 4.0 * geoLong) - eqTime + timeZone * 60



-- Sunrise in minutes, option = -1


sunRise mod =
    risetMns mod -1



-- Sunset  in minutes, option = +1


sunSet mod =
    risetMns mod 1


risetMns mod rsOption =
    getNoon mod + 4.0 * rsOption * getHA mod



-- Civil Twilight minutes since midnight


civTwlMns mod rsOption =
    getNoon mod + 4 * rsOption * getCivTwHA mod



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



-- Daylength as hours


getDayLength mod =
    getHA mod / 7.5



-- Sun Declination, OK tested 24.10.2019


sunDeclination mod =
    asinDeg (sinDeg (obliqCorr mod) * sinDeg (appLongSun mod))



-- True Solar Time


trueSolTime mod =
    let
        hr =
            getDecVar mod.hour

        mn =
            getDecVar mod.minute

        sc =
            getDecVar mod.second

        tz =
            getDecVar mod.timezone

        e2 =
            60.0 * (hr + tz) + mn + sc / 60.0

        v2 =
            equatTime mod

        b4 =
            getDecVar mod.longitude
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
            getDecVar mod.latitude

        t2 =
            sunDeclination mod
    in
    acosDeg (sinDeg b3 * sinDeg t2 + cosDeg b3 * cosDeg t2 * cosDeg (hourAngle mod))



--  Solar Azimuth angle clockwise from north, OK tested 19.11.2019


preAzimuth mod =
    let
        b3 =
            getDecVar mod.latitude

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



-- Atmospheric Refraction


atmosRefract : Model -> Float
atmosRefract mod =
    let
        solElev =
            90.0 - solZenith mod
    in
    if solElev > 85.0 then
        0.0

    else if solElev > 5.0 then
        (58.1
            / tanDeg solElev
            - 0.07
            / tanDeg solElev
            ^ 3
            + 8.6e-5
            / tanDeg solElev
            ^ 5
        )
            / 3600.0

    else if solElev > -0.575 then
        (1735.0
            + solElev
            * (-518.2
                + solElev
                * (103.4 + solElev * (-12.79 + solElev * 0.711))
              )
        )
            / 3600.0

    else
        -20.772 / tanDeg solElev / 3600.0


refractCorrectAltitude : Model -> Float
refractCorrectAltitude mod =
    90.0
        - solZenith mod
        + atmosRefract mod



-- Help functions
-- conversion radians to degrees


toRad =
    \alfa -> pi * alfa / 180.0



-- coversion degrees to radians


toDeg =
    \beta -> 180.0 * beta / pi


frac x =
    x - toFloat (floor x)



-- Determine Julian day from calendar date


getJDN mod =
    jdn (gI mod.year) (gI mod.month) (gI mod.day)


preCent xJDN yJD =
    toFloat xJDN + yJD


postCent xJDN yJD =
    (toFloat (xJDN - 2451545) + yJD) / 36525.0


fJD mod =
    (toFloat (gI mod.hour)
        - 12.0
        + toFloat (gI mod.minute)
        / 60.0
        + toFloat (gI mod.second)
        / 3600.0
    )
        / 24.0


gI element =
    Maybe.withDefault 0 (toInt element)


jdn : Int -> Int -> Int -> Int
jdn y m d =
    let
        w =
            (m - 14) // 12

        t =
            y + 4800 + w

        u =
            m - 2 - 12 * w

        v =
            (y + 4900 + w) // 100
    in
    (1461 * t)
        // 4
        + (367 * u)
        // 12
        - (3 * v)
        // 4
        + d
        - 32075


cosDeg alfa =
    cos (toRad alfa)


tanDeg alfa =
    tan (toRad alfa)


sinDeg alfa =
    sin (toRad alfa)


asinDeg =
    \x -> toDeg (asin x)


decNorm360 =
    \arg -> toFloat (remainderBy 360 (floor arg)) + frac arg


acosDeg z =
    if z > 1.0 then
        0.0

    else if z < -1.0 then
        180.0

    else
        acos z |> toDeg


atan2Deg =
    \x y -> toDeg (atan2 x y)



-- VIEW


xLabel s =
    H.label
        [ HA.style "padding" "5px"
        , HA.style "background-color" "blue"
        , HA.style "color" "white"
        ]
        [ text s ]


view : Model -> View Msg
view model =
    { title = "Sun-calculaor"
    , body =
        UI.layout
            [ H.h1 [] [ text "Sun Position Calculator" ]
            , div []
                [ xLabel "Year"
                , viewInput "number" "Give year" model.year Year
                , xLabel "Month"
                , viewInput "number" "Month" model.month Month
                , xLabel "Day"
                , viewInput "number" "Day" model.day Daynumber
                , xLabel "Hours UTC"
                , viewInput "number" "Hour" model.hour Hour
                , xLabel "Minutes"
                , viewInput "number" "Minute" model.minute Minute
                , xLabel "Seconds"
                , viewInput "number" "Second" model.second Second
                , H.h2 [ HA.style "color" "black" ] [ text "Location " ]
                , xLabel "Latitude"
                , viewInput "text" "Latitude" model.latitude Latitude
                , xLabel "Longitude"
                , viewInput "text" "Longitude" model.longitude Longitude
                , xLabel "Timezone"
                , viewInput "number" "Timezone" model.timezone Timezone
                , viewValidation model
                , viewResults model
                , viewJD model
                , viewDeclination model
                , viewFooter model
                ]
            ]
    }


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    H.input
        [ HA.type_ t
        , HA.placeholder p
        , HA.value v
        , HA.style "width" "55px"
        , onInput toMsg
        ]
        []


viewValidation : Model -> Html msg
viewValidation model =
    if
        getInputValue model.month
            > 0
            && getInputValue model.month
            < 13
            && getInputValue model.day
            > 0
            && getInputValue model.day
            < 32
    then
        div [ HA.style "color" "blue" ] [ text "Date entry OK" ]

    else
        div [ HA.style "color" "red" ] [ text "Incorrect month or day" ]


getDecVar : String -> Float
getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


getInputValue : String -> Int
getInputValue laji =
    Maybe.withDefault 0 (toInt laji)


viewJD : Model -> Html msg
viewJD model =
    div
        [ HA.style "color" "black"
        , HA.style "background-color" "lightblue"
        , HA.style "padding" "2em"
        ]
        [ p [] [ text ("JDN " ++ fromInt (getJDN model)) ]
        , text (" JD = " ++ cutDec6 (preCent (getJDN model) (fJD model)))
        , text (" Century = " ++ String.fromFloat (getCentury model))
        , p [] [ text (" Right Ascension      = " ++ cutDec6 (rectAsc model)) ]
        , p [] [ text (" Equation of Time     = " ++ cutDec6 (equatTime model)) ]
        , p [] [ text (" Sunrise HA           = " ++ cutDec6 (getHA model)) ]
        , p [] [ text (" Sunrise Civil Twilight HA  = " ++ cutDec6 (getCivTwHA model)) ]
        , p [] [ text (" True Solar Time      = " ++ cutDec6 (trueSolTime model)) ]
        , p [] [ text (" Hour Angle           = " ++ cutDec6 (hourAngle model)) ]
        , p [] [ text (" Solar Zenith         = " ++ cutDec6 (solZenith model)) ]
        ]


viewDeclination : Model -> Html msg
viewDeclination model =
    div [ HA.style "color" "blue", HA.style "font-size" "1.4em" ]
        [ p [] [ text (" Sun Declination   = " ++ cutDec6 (sunDeclination model) ++ "°") ]
        , p [] [ text (" Day Length        = " ++ formTime (getDayLength model)) ]
        , p [] [ text (" Civil Twilight    = " ++ mnToHrMn (civTwlMns model -1) ++ locTZ model) ]
        , p [] [ text (morningToNoon model) ]
        , p [] [ text (" Noon Time         = " ++ mnToHrMn (getNoon model) ++ locTZ model) ]
        , p [] [ text (noonToEvening model) ]
        , p [] [ text (" Civil Twilight    = " ++ mnToHrMn (civTwlMns model 1) ++ locTZ model) ]
        , p [] [ text (" Solar Azimuth     = " ++ cutDec4 (solAzimuth model) ++ "°") ]
        , p [] [ text (" Air refraction    = " ++ cutDec4 (atmosRefract model) ++ "°") ]
        , p []
            [ text
                (" Sun Altitude      = "
                    ++ cutDec4 (90.0 - solZenith model)
                    ++ "°  without air-refraction"
                )
            ]
        , p []
            [ text
                (" Sun Altitude      = "
                    ++ cutDec4 (refractCorrectAltitude model)
                    ++ "° Corrected with air-refraction"
                )
            ]
        ]


viewResults : Model -> Html msg
viewResults model =
    div
        [ HA.style "color" "yellow"
        , HA.style "background-color" "darkCyan"
        , HA.style "padding" "10px"
        ]
        [ H.h2 [] [ H.text "DATE AND LOCATION USED BELOW" ]
        , p []
            [ H.text
                ("Year  "
                    ++ model.year
                    ++ ", month    "
                    ++ model.month
                    ++ " and day  "
                    ++ model.day
                )
            ]
        , p []
            [ H.text
                (" Hours UTC "
                    ++ model.hour
                    ++ " Minutes "
                    ++ model.minute
                    ++ " Seconds "
                    ++ model.second
                )
            ]
        , p []
            [ H.text
                ("  Latitude "
                    ++ model.latitude
                    ++ "°  Longitude "
                    ++ model.longitude
                    ++ "°  Timezone "
                    ++ model.timezone
                    ++ " h"
                )
            ]
        ]


morningToNoon mod =
    let
        a1 =
            " Sunrise Time      = " ++ mnToHrMn (sunRise mod) ++ locTZ mod

        a2 =
            " Arctic winter, no sunrise"

        a3 =
            " Arctic summer, no sunset"

        a4 =
            " Antarctic midsummer, no sunset"

        a0 =
            " Daylength Exception: "

        geoLat =
            getDecVar mod.latitude

        declination =
            sunDeclination mod

        dayLength =
            getDayLength mod
    in
    if declination < 0.0 && geoLat > (90.83 + declination) then
        a2

    else if declination < 0.0 && geoLat < (-89.17 - declination) then
        a4

    else if declination > 0.0 && geoLat > (89.17 - declination) then
        a3

    else if dayLength > 0.0 && dayLength < 24.0 then
        a1

    else
        a0 ++ String.fromFloat dayLength


noonToEvening mod =
    let
        a1 =
            " Sunset Time      = " ++ mnToHrMn (sunSet mod) ++ locTZ mod

        a2 =
            " Arctic winter, no Sunrise"

        a3 =
            " Polar summer, no sunset"

        a4 =
            " No Sunset, Summer in Antarctis"

        geoLat =
            getDecVar mod.latitude

        declination =
            sunDeclination mod
    in
    if declination < 0.0 && geoLat > (90.8 + declination) then
        a2

    else if declination < 0.0 && geoLat < -89.2 - declination then
        a4

    else if declination > 0.0 && geoLat > (89.2 - declination) then
        a3

    else
        a1


viewFooter : Model -> Html msg
viewFooter model =
    div [ HA.style "color" "black", HA.style "font-size" "1.2em" ]
        [ p [ HA.style "margin-right" "50%" ]
            [ H.text
                """
                 This programm contains the basic functions
                 to be used in calculation of solar positions,
                 Sunrise and Sunset times for the given
                 geographic location, date and time.

                 The code is written in Elm-programming language
                 version 0.19.1 and built with elm-spa tool

                """
            ]
        , p [HA.style "padding-top" "24px" ]
             [ text " © Jarmo Lammi 2019 - 2023"]
        ]


locTZ mod =
    " UTC +" ++ mod.timezone ++ " h local time"


formTime : Float -> String
formTime x =
    let
        hrs =
            floor x

        mns =
            (60 * (x - toFloat hrs)) |> floor

        sec =
            60 * ((60 * (x - toFloat hrs)) - toFloat mns) |> round
    in
    zeroFill hrs ++ ":" ++ zeroFill mns ++ ":" ++ zeroFill sec


zeroFill : Int -> String
zeroFill x =
    if x < 10 then
        "0" ++ String.fromInt x

    else
        String.fromInt x



-- Cut to six decimals format of Float-numbers


cutDecNum : Float -> Int -> String
cutDecNum nr ndecim =
    let
        snr =
            if abs nr > 1.0e-6 then
                String.fromFloat nr

            else
                "0.000000"

        dotIndex =
            String.indices "." snr

        dotNr =
            Maybe.withDefault 0 (List.minimum dotIndex)

        decNrLength =
            String.length snr

        decimPart =
            String.slice dotNr decNrLength snr

        cutTo6 =
            String.left ndecim decimPart

        intPart =
            String.left dotNr snr
    in
    intPart ++ cutTo6


cutDec6 nr =
    cutDecNum nr 7


cutDec4 nr =
    cutDecNum nr 5
