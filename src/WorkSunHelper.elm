module WorkSunHelper exposing (..)

import GregorJDN exposing (jdateGr, jdnGr)
import MnToHrMnSc exposing (mnToHrMn)
import String exposing (fromFloat, fromInt, toInt)



-- Sun true anomality, OK tested 21.10.19
-- sunTrueAnom cent =
-- meanAnomalSun cent + sunEqCntr cent
-- MODEL


type alias Inputdata =
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


init : Inputdata
init =
    Inputdata "2022" "08" "28" "10" "24" "31" "65.85" "24.18" "2"


sunEqCntr : Float -> Float
sunEqCntr cent =
    sinDeg (meanAnomalSun cent)
        * (1.914602 - cent * (0.004817 + 0.000014 * cent))
        + sinDeg (2.0 * meanAnomalSun cent)
        * (0.019993 - 0.000101 * cent)
        + sinDeg (3.0 * meanAnomalSun cent)
        * 0.000289



-- Eccentricy of Earth Orbit, OK tested 11.11.2019


eccentEarthOrbit cent =
    0.016708634
        - cent
        * (4.2037e-5 + 1.267e-7 * cent)



-- Mean anomality of Sun, OK tested 10.11.2019


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


getCentury inputdata =
    getCent (fJD inputdata)



-- Mean Longitude of Sun, OK tested 10.11.2019


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


trueLongSun : Inputdata -> Float
trueLongSun inputdata =
    let
        cent =
            getCentury inputdata
    in
    calcSunML cent + sunEqCntr cent



-- Sun apparent longitude, OK tested 22.10.19


appLongSun : Inputdata -> Float
appLongSun inputdata =
    trueLongSun inputdata - 5.69e-3 - 4.78e-3 * sinDeg (125.04 - 1934.136 * getCentury inputdata)



-- Mean Obliquity of Ecliptic


meanObliqEclip : Inputdata -> Float
meanObliqEclip inputdata =
    let
        cent =
            getCentury inputdata
    in
    23.0 + (26.0 + (21.448 - cent * (46.815 + cent * (5.9e-4 - cent * 1.813e-3))) / 60.0) / 60.0



-- Corrected obliquity, OK 22.10.19


obliqCorr : Inputdata -> Float
obliqCorr inputdata =
    meanObliqEclip inputdata + 0.00256 * cosDeg (125.04 - 1934.136 * getCentury inputdata)



-- Right Ascension RA, OK tested 14.11.2019


rectAsc : Inputdata -> Float
rectAsc inputdata =
    let
        appLongS =
            appLongSun inputdata
    in
    atan2Deg (cosDeg (obliqCorr inputdata) * sinDeg appLongS) (cosDeg appLongS)



-- Used in Equation of Time


variableY : Inputdata -> Float
variableY inputdata =
    let
        x =
            tanDeg (obliqCorr inputdata / 2.0)
    in
    x * x



-- Equation of Time, OK tested 15.11.2019


equatTime : Inputdata -> Float
equatTime inputdata =
    let
        cent =
            getCentury inputdata

        varY =
            variableY inputdata

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


srHA inputdata zenith =
    let
        geoLat =
            getDecVar inputdata.latitude

        declination =
            sunDeclination inputdata

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


getHA inputdata =
    srHA inputdata 90.833



-- Civil twilight Sunrise HA


getCivTwHA inputdata =
    srHA inputdata 96.0



-- Noon time as minutes since midnight
-- OK tested 01.11.2019


getNoon inputdata =
    let
        geoLong =
            getDecVar inputdata.longitude

        timeZone =
            getDecVar inputdata.timezone

        eqTime =
            equatTime inputdata
    in
    (720.0 - 4.0 * geoLong) - eqTime + timeZone * 60



-- Sunrise in minutes, option = -1


sunRise inputdata =
    risetMns inputdata -1



-- Sunset  in minutes, option = +1


sunSet inputdata =
    risetMns inputdata 1


risetMns inputdata rsOption =
    getNoon inputdata + 4.0 * rsOption * getHA inputdata



-- Civil Twilight minutes since midnight


civTwlMns inputdata rsOption =
    getNoon inputdata + 4 * rsOption * getCivTwHA inputdata



-- Daylength as hours


getDayLength inputdata =
    getHA inputdata / 7.5



-- Sun Declination, OK tested 24.10.2019


sunDeclination inputdata =
    asinDeg (sinDeg (obliqCorr inputdata) * sinDeg (appLongSun inputdata))



-- True Solar Time, OK tested 17.11.2019


trueSolTime inputdata =
    let
        hr =
            getDecVar inputdata.hour

        mn =
            getDecVar inputdata.minute

        sc =
            getDecVar inputdata.second

        tz =
            getDecVar inputdata.timezone

        e2 =
            60.0 * (hr + tz) + mn + sc / 60.0

        v2 =
            equatTime inputdata

        b4 =
            getDecVar inputdata.longitude
    in
    e2 + v2 + 4.0 * b4 - 60.0 * tz



-- Hour Angle degr. OK tested 17.11.2019


hourAngle inputdata =
    let
        tSt =
            trueSolTime inputdata
    in
    if tSt > 0.0 then
        0.25 * tSt - 180.0

    else
        0.25 * tSt + 180.0



-- Solar Zenith (degrees)


solZenith inputdata =
    let
        b3 =
            getDecVar inputdata.latitude

        t2 =
            sunDeclination inputdata
    in
    acosDeg (sinDeg b3 * sinDeg t2 + cosDeg b3 * cosDeg t2 * cosDeg (hourAngle inputdata))



--  Solar Azimuth angle clockwise from north, OK tested 19.11.2019


preAzimuth inputdata =
    let
        b3 =
            getDecVar inputdata.latitude

        ad =
            solZenith inputdata

        t =
            sunDeclination inputdata
    in
    acosDeg ((sinDeg b3 * cosDeg ad - sinDeg t) / (cosDeg b3 * sinDeg ad))


solAzimuth inputdata =
    let
        preAz =
            preAzimuth inputdata

        ac =
            hourAngle inputdata
    in
    if ac > 0.0 then
        preAz + 180.0 |> decNorm360

    else
        540.0 - preAz |> decNorm360



-- Atmospheric Refraction


atmosRefract inputdata =
    let
        solElev =
            90.0 - solZenith inputdata
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


refractCorrectAltitude inputdata =
    90.0
        - solZenith inputdata
        + atmosRefract inputdata


locTZ inputdata =
    " UTC + " ++ inputdata.timezone ++ " h local time"


morningToNoon inputdata =
    let
        a1 =
            " Sunrise Time      = " ++ mnToHrMn (sunRise inputdata) ++ locTZ inputdata

        a2 =
            " Arctic winter, no sunrise"

        a3 =
            " Arctic summer, no sunset"

        a4 =
            " Antarctic midsummer, no sunset"

        a0 =
            " Daylength Exception: "

        geoLat =
            getDecVar inputdata.latitude

        declination =
            sunDeclination inputdata

        dayLength =
            getDayLength inputdata
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


noonToEvening inputdata =
    let
        a1 =
            " Sunset Time      = " ++ mnToHrMn (sunSet inputdata) ++ locTZ inputdata

        a2 =
            " Arctic winter, no Sunrise"

        a3 =
            " Polar summer, no sunset"

        a4 =
            " No Sunset, Summer in Antarctis"

        geoLat =
            getDecVar inputdata.latitude

        declination =
            sunDeclination inputdata
    in
    if declination < 0.0 && geoLat > (90.8 + declination) then
        a2

    else if declination < 0.0 && geoLat < -89.2 - declination then
        a4

    else if declination > 0.0 && geoLat > (89.2 - declination) then
        a3

    else
        a1



-- Help functions


toRad =
    \alfa -> pi * alfa / 180.0



-- coversion degrees to radians


toDeg =
    \beta -> 180.0 * beta / pi



-- conversion radians to degrees


cosDeg alfa =
    cos (toRad alfa)


tanDeg alfa =
    tan (toRad alfa)


sinDeg alfa =
    sin (toRad alfa)


asinDeg =
    \x -> toDeg (asin x)



-- acosDeg =
--    \x -> toDeg (acos x)
-- This will never be NaN:


acosDeg z =
    if z > 1.0 then
        0.0

    else if z < -1.0 then
        180.0

    else
        acos z |> toDeg


atan2Deg =
    \x y -> toDeg (atan2 x y)


decNorm360 =
    \arg -> toFloat (remainderBy 360 (floor arg)) + frac arg


frac x =
    x - toFloat (floor x)



-- Determine Julian day from calendar date


getJDN inputdata =
    jdnGr (gI inputdata.year) (gI inputdata.month) (gI inputdata.day)


getCent : Float -> Float
getCent yJD =
    let
        jD2000 =
            toFloat (jdnGr 2000 1 1)
    in
    (yJD - jD2000) / 36525.0


fJD :
    { a
        | day : String
        , hour : String
        , minute : String
        , month : String
        , second : String
        , year : String
    }
    -> Float
fJD inputdata =
    let
        y =
            gI inputdata.year

        m =
            gI inputdata.month

        d =
            gI inputdata.day

        h =
            gI inputdata.hour

        mn =
            gI inputdata.minute

        s =
            gI inputdata.second
    in
    jdateGr y m d h mn s


gI element =
    Maybe.withDefault 0 (toInt element)


getDecVar : String -> Float
getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


getInputValue : String -> Int
getInputValue laji =
    Maybe.withDefault 0 (toInt laji)
