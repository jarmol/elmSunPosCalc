module SunHelper exposing (..)
import CommonModel exposing (InputData)
import GregorJDN exposing (jdateGr, jdnGr)
import MnToHrMnSc exposing (mnToHrMn)   -- Converts minutes of day to usual time hr:mn:sc



-- MODEL

type alias InputData = CommonModel.InputData



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



getCentury : InputData -> Float
getCentury inputData =
    getCent (fJD inputData)



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


trueLongSun : InputData -> Float
trueLongSun inputData =
    let
        cent =
            getCentury inputData
    in
    calcSunML cent + sunEqCntr cent



-- Sun apparent longitude, OK tested 22.10.19


appLongSun : InputData -> Float
appLongSun inputData =
    trueLongSun inputData - 5.69e-3 - 4.78e-3 * sinDeg (125.04 - 1934.136 * getCentury inputData)



-- Mean Obliquity of Ecliptic


meanObliqEclip : InputData -> Float
meanObliqEclip inputData =
    let
        cent =
            getCentury inputData
    in
    23.0 + (26.0 + (21.448 - cent * (46.815 + cent * (5.9e-4 - cent * 1.813e-3))) / 60.0) / 60.0



-- Corrected obliquity, OK 22.10.19


obliqCorr : InputData -> Float
obliqCorr inputData =
    meanObliqEclip inputData + 0.00256 * cosDeg (125.04 - 1934.136 * getCentury inputData)



-- Right Ascension RA, OK tested 14.11.2019


rectAsc : InputData -> Float
rectAsc inputData =
    let
        appLongS =
            appLongSun inputData
    in
    atan2Deg (cosDeg (obliqCorr inputData) * sinDeg appLongS) (cosDeg appLongS)



-- Used in Equation of Time


variableY : InputData -> Float
variableY inputData =
    let
        x =
            tanDeg (obliqCorr inputData / 2.0)
    in
    x * x



-- Equation of Time, OK tested 15.11.2019


equatTime : InputData -> Float
equatTime inputData =
    let
        cent =
            getCentury inputData

        varY =
            variableY inputData

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


srHA inputData zenith =
    let
        geoLat =
            getDecVar inputData.latitude

        declination =
            sunDeclination inputData

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


getHA inputData =
    srHA inputData 90.833



-- Civil twilight Sunrise HA


getCivTwHA inputData =
    srHA inputData 96.0



-- Noon time as minutes since midnight
-- OK tested 01.11.2019


getNoon : InputData -> Float
getNoon inputData =
    let
        geoLong =
            getDecVar inputData.longitude

        timeZone =
            getDecVar inputData.timezone

        eqTime =
            equatTime inputData
    in
    (720.0 - 4.0 * geoLong) - eqTime + timeZone * 60



-- Sunrise in minutes, option = -1


sunRise inputData =
    risetMns inputData -1



-- Sunset  in minutes, option = +1


sunSet inputData =
    risetMns inputData 1


risetMns inputData rsOption =
    getNoon inputData + 4.0 * rsOption * getHA inputData



-- Civil Twilight minutes since midnight


civTwlMns inputData rsOption =
    getNoon inputData + 4 * rsOption * getCivTwHA inputData



-- Daylength as hours


getDayLength inputData =
    getHA inputData / 7.5



-- Sun Declination, OK tested 24.10.2019

sunDeclination inputData =
    let 
        appLS = sinDeg <|appLongSun inputData
        oblC  = sinDeg <| obliqCorr inputData
    in
        asinDeg (oblC * appLS)


-- True Solar Time, OK tested 17.11.2019


trueSolTime inputData =
    let
        hr =
            getDecVar inputData.hour

        mn =
            getDecVar inputData.minute

        sc =
            getDecVar inputData.second

        tz =
            getDecVar inputData.timezone

        e2 =
            60.0 * (hr + tz) + mn + sc / 60.0

        v2 =
            equatTime inputData

        b4 =
            getDecVar inputData.longitude
    in
    e2 + v2 + 4.0 * b4 - 60.0 * tz



-- Hour Angle degr. OK tested 17.11.2019


hourAngle inputData =
    let
        tSt =
            trueSolTime inputData
    in
    if tSt > 0.0 then
        0.25 * tSt - 180.0

    else
        0.25 * tSt + 180.0



-- Solar Zenith (degrees)


solZenith inputData =
    let
        b3 =
            getDecVar inputData.latitude

        t2 =
            sunDeclination inputData
    in
    acosDeg (sinDeg b3 * sinDeg t2 + cosDeg b3 * cosDeg t2 * cosDeg (hourAngle inputData))



--  Solar Azimuth angle clockwise from north, OK tested 19.11.2019


preAzimuth inputData =
    let
        b3 =
            getDecVar inputData.latitude

        ad =
            solZenith inputData

        t =
            sunDeclination inputData
    in
    acosDeg ((sinDeg b3 * cosDeg ad - sinDeg t) / (cosDeg b3 * sinDeg ad))


solAzimuth inputData =
    let
        preAz =
            preAzimuth inputData

        ac =
            hourAngle inputData
    in
    if ac > 0.0 then
        preAz + 180.0 |> decNorm360

    else
        540.0 - preAz |> decNorm360



-- Atmospheric Refraction


atmosRefract inputData =
    let
        solElev =
            90.0 - solZenith inputData
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


refractCorrectAltitude inputData =
    90.0
        - solZenith inputData
        + atmosRefract inputData


locTZ inputData =
    " UTC + " ++ inputData.timezone ++ " h local time"


morningToNoon inputData =
    let
        a1 =
            " Sunrise Time      = " ++ mnToHrMn (sunRise inputData) ++ locTZ inputData

        a2 =
            " Arctic winter, no sunrise"

        a3 =
            " Arctic summer, no sunset"

        a4 =
            " Antarctic midsummer, no sunset"

        a0 =
            " Daylength Exception: "

        geoLat =
            getDecVar inputData.latitude

        declination =
            sunDeclination inputData

        dayLength =
            getDayLength inputData
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


noonToEvening inputData =
    let
        a1 =
            " Sunset Time      = " ++ mnToHrMn (sunSet inputData) ++ locTZ inputData

        a2 =
            " Arctic winter, no Sunrise"

        a3 =
            " Polar summer, no sunset"

        a4 =
            " No Sunset, Summer in Antarctis"

        geoLat =
            getDecVar inputData.latitude

        declination =
            sunDeclination inputData
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


-- coversion degrees to radians

toRad =
    \alfa -> pi * alfa / 180.0



-- conversion radians to degrees

toDeg =
    \beta -> 180.0 * beta / pi



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

-- Convert decimal hour to time string hh:mm:ss

decHrToTime: Float -> String
decHrToTime dechr =
    mnToHrMn (60 * dechr)


-- Determine Julian day from calendar date

getJDN : InputData -> Int
getJDN inputData =
    jdnGr (gI inputData.year) (gI inputData.month) (gI inputData.day)


getCent : Float -> Float
getCent yJD =
    let
        jD2000 =
            toFloat (jdnGr 2000 1 1)
    in
    (yJD - jD2000) / 36525.0


   
fJD : InputData -> Float
fJD inputData =
    let
        y =
            gI inputData.year

        m =
            gI inputData.month

        d =
            gI inputData.day

        h =
            gI inputData.hour

        mn =
            gI inputData.minute

        s =
            gI inputData.second
    in
    jdateGr y m d h mn s


gI element =
    Maybe.withDefault 0 (String.toInt element)


getDecVar : String -> Float
getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


getInputValue : String -> Int
getInputValue laji =
    Maybe.withDefault 0 (String.toInt laji)
