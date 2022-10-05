module SunHelper exposing (InputData, atmosRefract, civTwlMns, decHrToTime, fJD, getDayLength, getDecVar, getInputValue, getJDN, getNoon, refractCorrectAltitude, solAzimuth, solZenith, sunDeclination, sunRise, sunSet)

import CommonModel exposing (InputData)
import GregorJDN exposing (jdateGr, jdnGr)
import MnToHrMnSc exposing (mnToHrMn)



-- Converts minutes of day to usual time hr:mn:sc
-- MODEL


type alias InputData =
    CommonModel.InputData


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
        a : Float
        a =
            357.52911

        b : Float
        b =
            35999.05029

        c : Float
        c =
            1.537e-4
    in
    a + cent * (b + cent * c) |> decNorm360


getCentury : InputData -> Float
getCentury inputData =
    getCent (fJD inputData)



-- Mean Longitude of Sun, OK tested 10.11.2019


calcSunML : Float -> Float
calcSunML cent =
    let
        a : Float
        a =
            280.46646

        b : Float
        b =
            36000.76983

        c : Float
        c =
            3.032e-4
    in
    a + cent * (b + cent * c) |> decNorm360



-- Sun true longitude, OK tested 12.11.2019


trueLongSun : InputData -> Float
trueLongSun inputData =
    let
        cent : Float
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
        cent : Float
        cent =
            getCentury inputData
    in
    23.0 + (26.0 + (21.448 - cent * (46.815 + cent * (5.9e-4 - cent * 1.813e-3))) / 60.0) / 60.0



-- Corrected obliquity, OK 22.10.19


obliqCorr : InputData -> Float
obliqCorr inputData =
    meanObliqEclip inputData + 0.00256 * cosDeg (125.04 - 1934.136 * getCentury inputData)



-- Right Ascension RA, OK tested 14.11.2019
-- Used in Equation of Time


variableY : InputData -> Float
variableY inputData =
    let
        x : Float
        x =
            tanDeg (obliqCorr inputData / 2.0)
    in
    x * x



-- Equation of Time, OK tested 15.11.2019


equatTime : InputData -> Float
equatTime inputData =
    let
        cent : Float
        cent =
            getCentury inputData

        varY : Float
        varY =
            variableY inputData

        meanLongS : Float
        meanLongS =
            calcSunML cent

        eOrbitEx : Float
        eOrbitEx =
            eccentEarthOrbit cent

        meanAnomS : Float
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


srHA : InputData -> Float -> Float
srHA inputData zenith =
    let
        geoLat : Float
        geoLat =
            getDecVar inputData.latitude

        declination : Float
        declination =
            sunDeclination inputData

        x : Float
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


getHA : InputData -> Float
getHA inputData =
    srHA inputData 90.833



-- Civil twilight Sunrise HA


getCivTwHA : InputData -> Float
getCivTwHA inputData =
    srHA inputData 96.0



-- Noon time as minutes since midnight
-- OK tested 01.11.2019


getNoon : InputData -> Float
getNoon inputData =
    let
        geoLong : Float
        geoLong =
            getDecVar inputData.longitude

        timeZone : Float
        timeZone =
            getDecVar inputData.timezone

        eqTime : Float
        eqTime =
            equatTime inputData
    in
    (720.0 - 4.0 * geoLong) - eqTime + timeZone * 60



-- Sunrise in minutes, option = -1

sunRise : InputData -> Float
sunRise inputData =
    risetMns inputData -1



-- Sunset  in minutes, option = +1

sunSet : InputData -> Float
sunSet inputData =
    risetMns inputData 1


risetMns : InputData -> Int -> Float
risetMns inputData rsOption =
    getNoon inputData + 4.0 * toFloat rsOption * getHA inputData



-- Civil Twilight minutes since midnight


civTwlMns : InputData -> Int -> Float
civTwlMns inputData rsOption =
    getNoon inputData + 4 * toFloat rsOption * getCivTwHA inputData



-- Daylength as hours

getDayLength : InputData -> Float
getDayLength inputData =
    getHA inputData / 7.5



-- Sun Declination, OK tested 24.10.2019


sunDeclination : InputData -> Float
sunDeclination inputData =
    let
        appLS : Float
        appLS =
            sinDeg <| appLongSun inputData

        oblC : Float
        oblC =
            sinDeg <| obliqCorr inputData
    in
    asinDeg (oblC * appLS)



-- True Solar Time, OK tested 17.11.2019


trueSolTime : InputData -> Float
trueSolTime inputData =
    let
        hr : Float
        hr =
            getDecVar inputData.hour

        mn : Float
        mn =
            getDecVar inputData.minute

        sc : Float
        sc =
            getDecVar inputData.second

        tz : Float
        tz =
            getDecVar inputData.timezone

        e2 : Float
        e2 =
            60.0 * (hr + tz) + mn + sc / 60.0

        v2 : Float
        v2 =
            equatTime inputData

        b4 : Float
        b4 =
            getDecVar inputData.longitude
    in
    e2 + v2 + 4.0 * b4 - 60.0 * tz



-- Hour Angle degr. OK tested 17.11.2019


hourAngle : InputData -> Float
hourAngle inputData =
    let
        tSt : Float
        tSt =
            trueSolTime inputData
    in
    if tSt > 0.0 then
        0.25 * tSt - 180.0

    else
        0.25 * tSt + 180.0



-- Solar Zenith (degrees)

solZenith : InputData -> Float
solZenith inputData =
    let
        b3 : Float
        b3 =
            getDecVar inputData.latitude

        t2 : Float
        t2 =
            sunDeclination inputData
    in
    acosDeg (sinDeg b3 * sinDeg t2 + cosDeg b3 * cosDeg t2 * cosDeg (hourAngle inputData))



--  Solar Azimuth angle clockwise from north, OK tested 19.11.2019

preAzimuth : InputData -> Float
preAzimuth inputData =
    let
        b3 : Float
        b3 =
            getDecVar inputData.latitude

        ad : Float
        ad =
            solZenith inputData

        t : Float
        t =
            sunDeclination inputData
    in
    acosDeg ((sinDeg b3 * cosDeg ad - sinDeg t) / (cosDeg b3 * sinDeg ad))


solAzimuth : InputData -> Float
solAzimuth inputData =
    let
        preAz : Float
        preAz =
            preAzimuth inputData

        ac : Float
        ac =
            hourAngle inputData
    in
    if ac > 0.0 then
        preAz + 180.0 |> decNorm360

    else
        540.0 - preAz |> decNorm360



-- Atmospheric Refraction

atmosRefract : InputData -> Float
atmosRefract inputData =
    let
        solElev : Float
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


refractCorrectAltitude : InputData -> Float
refractCorrectAltitude inputData =
    90.0
        - solZenith inputData
        + atmosRefract inputData



-- Help functions
-- coversion degrees to radians

toRad : Float -> Float
toRad =
    \alfa -> pi * alfa / 180.0


-- conversion radians to degrees

toDeg : Float -> Float
toDeg =
    \beta -> 180.0 * beta / pi


cosDeg : Float ->Float
cosDeg alfa =
    cos (toRad alfa)


tanDeg : Float -> Float
tanDeg alfa =
    tan (toRad alfa)


sinDeg : Float -> Float
sinDeg alfa =
    sin (toRad alfa)


asinDeg : Float -> Float
asinDeg =
    \x -> toDeg (asin x)



-- acosDeg =
--    \x -> toDeg (acos x)
-- This will never be NaN:

acosDeg : Float -> Float
acosDeg z =
    if z >= 1.0 then
        0.0

    else if z < -1.0 then
        180.0

    else
        acos z |> toDeg


decNorm360 : Float -> Float
decNorm360 =
    \arg -> toFloat (remainderBy 360 (floor arg)) + frac arg


frac : Float -> Float
frac x =
    x - toFloat (floor x)



-- Convert decimal hour to time string hh:mm:ss


decHrToTime : Float -> String
decHrToTime dechr =
    mnToHrMn (60 * dechr)



-- Determine Julian day from calendar date


getJDN : InputData -> Int
getJDN inputData =
    jdnGr (gI inputData.year) (gI inputData.month) (gI inputData.day)


getCent : Float -> Float
getCent yJD =
    let
        jD2000 : Float
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
fJD inputData =
    let
        y : Int
        y =
            gI inputData.year
        
        m : Int
        m = 
            gI inputData.month

        d: Int
        d =
            gI inputData.day

        h : Int
        h =
            gI inputData.hour

        mn : Int
        mn = 
            gI inputData.minute

        s : Int
        s =
            gI inputData.second
    in
    jdateGr y m d h mn s


gI : String -> Int
gI element =
    Maybe.withDefault 0 (String.toInt element)


getDecVar : String -> Float
getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


getInputValue : String -> Int
getInputValue laji =
    Maybe.withDefault 0 (String.toInt laji)
