import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (fromFloat, fromInt, toInt)

-- File saved in https://ellie-app.com/7drvTVwxKGVa1
-- and https://ellie-app.com/7drL34ytZTWa1
-- Calculates some parametres of the solar position
-- Converts Calendar date to Julian Day Number JDN
-- This code is compatible with Elm 0.19.0
-- Author: Jarmo Lammi
-- © 2019  Jarmo Lammi


-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { year : String
    , month : String
    , day : String
    , hour : String
    , minute : String
    , latitude : String
    , longitude : String
    , timezone : String
    }


init : Model
init =
    Model "2019" "09" "16" "10" "18" "65.85" "24.18" "2"


wCalculated =
    { iJD = 1.0, iJDN = 1, iCent = 0.197 }



-- UPDATE


type Msg
    = Year String
    | Month String
    | Daynumber String
    | Hour String
    | Minute String
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

        Latitude latitude ->
            { model | latitude = latitude }

        Longitude longitude ->
            { model | longitude = longitude }

        Timezone timezone ->
            { model | timezone = timezone }



-- Sun true anomality, OK tested 21.10.19


sunTrueAnom cent =
    meanAnomalSun cent + sunEqCntr cent


sunEqCntr : Float -> Float
sunEqCntr cent =
    let
        mAnomalSun =
            meanAnomalSun cent
    in
    sinDeg mAnomalSun
        * (1.914602 - cent * (0.004817 + 0.000014 * cent))
        + sinDeg (2.0 * mAnomalSun)
        * (0.019993 - 0.000101 * cent)
        + sinDeg (3.0 * mAnomalSun)
        * 0.000289



-- Eccentricy of Earth Orbit, OK tested 11.11.2019


eccentEarthOrbit cent =
    let
        a =
            0.016708634

        b =
            4.2037e-5

        c =
            1.267e-7
    in
    a - cent * (b + c * cent)



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


getCentury mod =
    postCent (getJDN mod) (fJD mod)



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


trueLongSun cent =
    calcSunML cent + sunEqCntr cent



-- Sun apparent longitude, OK tested 22.10.19


appLongSun cent =
    trueLongSun cent - 5.69e-3 - 4.78e-3 * sinDeg (125.04 - 1934.136 * cent)



-- Mean Obliquity of Ecliptic


meanObliqEclip cent =
    23.0 + (26.0 + (21.448 - cent * (46.815 + cent * (5.9e-4 - cent * 1.813e-3))) / 60.0) / 60.0



-- Corrected obliquity, OK 22.10.19


obliqCorr : Float -> Float
obliqCorr cent =
    meanObliqEclip cent + 0.00256 * cosDeg (125.04 - 1934.136 * cent)



-- Right Ascension RA, OK tested 14.11.2019


rectAsc : Float -> Float
rectAsc cent =
    let
        oblCorr =
            obliqCorr cent

        appLongS =
            appLongSun cent
    in
    atan2Deg (cosDeg oblCorr * sinDeg appLongS) (cosDeg appLongS)



-- Used in Equation of Time


variableY cent =
    let
        x =
            tanDeg (obliqCorr cent / 2.0)
    in
    x * x



-- Equation of Time, OK tested 15.11.2019


equatTime cent =
    let
        varY =
            variableY cent

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


srHA cent geoLat =
    let
        zenith =
            90.833

        declination =
            sunDeclination cent
    in
    acosDeg
        (cosDeg zenith
            / (cosDeg geoLat * cosDeg declination)
            - (tanDeg geoLat * tanDeg declination)
        )


getHA mod =
    srHA (getCentury mod) (getDecVar mod.latitude)



-- Noon time as minutes since midnight
-- OK tested 01.11.2019


getNoon mod =
    let
        geoLong =
            getDecVar mod.longitude

        timeZone =
            getDecVar mod.timezone

        eqTime =
            equatTime (getCentury mod)
    in
    (720.0 - 4.0 * geoLong) - eqTime + timeZone * 60



-- Sunrise in minutes, option = -1
-- Sunset  in minutes, option = +1


risetMns mod rsOption =
    getNoon mod + 4 * rsOption * getHA mod



-- Converts minutes to hh:mn:ss


mnToHrMn : Float -> String
mnToHrMn mns =
    let
        lmins =
            remainderBy 60 (floor mns)

        lhrs =
            floor mns // 60

        lsec =
            remainderBy 60 (round (60.0 * mns))
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

sunDeclination : Float -> Float
sunDeclination cent =
    asinDeg (sinDeg (obliqCorr cent) * sinDeg (appLongSun cent))


 
-- True Solar Time, OK tested 17.11.2019

trueSolTime mod cent =  
    let
        hr = getDecVar mod.hour
        mn = getDecVar mod.minute
        tz = getDecVar mod.timezone 
        e2 = 60.0*( hr + tz ) + mn 
        v2 = equatTime cent
        b4 = getDecVar mod.longitude
    in   
        e2 + v2 + 4.0 * b4 - 60.0 * tz 


-- Hour Angle degr. OK tested 17.11.2019

hourAngle mod cent =    
    let
        tSt = trueSolTime mod cent
    in 
        if tSt > 0.0 then 0.25 * tSt - 180.0
        else 0.25 * tSt + 180.0


-- Solar Zenith (degrees)

solZenith mod cent =  
    let  
        b3  = getDecVar mod.latitude
        t2 =  sunDeclination cent
        hrA = hourAngle mod cent
    in   
        acosDeg(sinDeg(b3)*sinDeg(t2) + cosDeg(b3)*cosDeg(t2)*cosDeg(hrA))


--  Solar Azimuth angle clockwise from north, OK tested 19.11.2019 

preAzimuth mod cent = 
     let ac = hourAngle mod cent
         b3 = getDecVar mod.latitude
         ad = solZenith mod cent
         t  = sunDeclination cent
     in
         acosDeg ((sinDeg(b3)*cosDeg(ad) - sinDeg(t))/(cosDeg(b3)*sinDeg(ad)))


solAzimuth mod cent =
    let preAz = preAzimuth mod cent
        ac    = hourAngle mod cent
    in
        if ac > 0.0 then preAz + 180.0 |> decNorm360
        else 540.0 - preAz |> decNorm360
 
-- Building new code 

-- VIEW

view : Model -> Html Msg
view model =
    div [ style "margin-left" "10%", style "margin-right" "20%" ]
        [ h1 [] [ text "Sun Position Calculator" ]
        , span [ style "background-color" "blue", style "color" "white" ]
            [ text "  Year "
            , viewInput "number" "Give year" model.year Year
            , text "  Month    "
            , viewInput "number" "Month (1 ... 12)" model.month Month
            , text "  Day   "
            , viewInput "number" "Day" model.day Daynumber
            , text "  Hours UTC "
            , viewInput "number" "Hour" model.hour Hour
            , text "  Minutes  "
            , viewInput "number" "Minute" model.minute Minute
            , h2 [ style "color" "black" ] [ text "Location " ]
            , text " Latitude "
            , viewInput "text" "Latitude" model.latitude Latitude
            , text "  Longitude "
            , viewInput "text" "Longitude" model.longitude Longitude
            , text "  Timezone  "
            , viewInput "number" "Timezone" model.timezone Timezone
            , viewValidation model
            , viewResults model
            , viewJD model
            , viewDeclination model
            , viewFooter model
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, style "width" "45px", onInput toMsg ] []


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
        div [ style "color" "blue" ] [ text "Date entry OK" ]

    else
        div [ style "color" "red" ] [ text "Incorrect month or day" ]


viewResults : Model -> Html msg
viewResults model =
    div [ style "color" "orange", style "background-color" "black" ]
        [ h2 [] [ text "DATE AND LOCATION USED BELOW" ]
        , p []
            [ text
                ("Year  "
                    ++ model.year
                    ++ ", month    "
                    ++ model.month
                    ++ " and day  "
                    ++ model.day
                )
            ]
        , p [] [ text ("  Hours UTC " ++ model.hour ++ "   Minutes  " ++ model.minute) ]
        , p []
            [ text
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


viewJD : Model -> Html msg
viewJD model =
    div [ style "color" "red", style "background-color" "lightblue" ]
        [ p [] [ text ("JDN " ++ fromInt (getJDN model)) ]
        , text (" JD = " ++ cutDecNum (preCent (getJDN model) (fJD model)))
        , text (" Century = " ++ cutDecNum (getCentury model))
--      , p [] [ text (" Sun Mean Long = " ++ fromFloat (calcSunML (getCentury model))) ]
--      , p [] [ text (" Sun Mean Anomality = " ++ fromFloat (meanAnomalSun (getCentury model))) ]
--      , p [] [ text (" Eccentricity of Earth Orbit = " ++ fromFloat (eccentEarthOrbit (getCentury model))) ]
--      , p [] [ text (" Mean and True Anomality Difference  = " ++ fromFloat (sunEqCntr (getCentury model))) ]
--      , p [] [ text (" Sun True Anomality = " ++ fromFloat (sunTrueAnom (getCentury model))) ]
--      , p [] [ text (" True Longitude of Sun = " ++ fromFloat (trueLongSun (getCentury model))) ]
--      , p [] [ text (" Apparent Longitude of Sun = " ++ fromFloat (appLongSun (getCentury model))) ]
--      , p [] [ text (" Mean Obliquity of Ecliptic = " ++ fromFloat (meanObliqEclip (getCentury model))) ]
--      , p [] [ text (" Corrected Obliquity  = " ++ fromFloat (obliqCorr (getCentury model))) ]
        , p [] [ text (" Right Ascension      = " ++ cutDecNum (rectAsc (getCentury model))) ]
--      , p [] [ text (" Variable Y           = " ++ fromFloat (variableY (getCentury model))) ]
        , p [] [ text (" Equation of Time     = " ++ cutDecNum (equatTime (getCentury model))) ]
        , p [] [ text (" Sunrise HA           = " ++ cutDecNum (getHA model)) ]
        , p [] [ text (" True Solar Time      = " ++ cutDecNum (trueSolTime model (getCentury model))) ]
        , p [] [ text (" Hour Angle           = " ++ cutDecNum (hourAngle   model (getCentury model))) ]
        , p [] [ text (" Solar Zenith         = " ++ cutDecNum (solZenith   model (getCentury model))) ]
        ]


viewDeclination : Model -> Html msg
viewDeclination model =
    div [ style "color" "red", style "font-size" "1.4em" ]
        [ p [] [ text (" Sun Declination   = " ++ (cutDecNum (sunDeclination (getCentury model))) ++ "°")]
        , p [] [ text (" Day Length        = " ++ formTime  (getDayLength model)) ]
        , p [] [ text (" Noon Time         = " ++ mnToHrMn  (getNoon model) ++ locTZ model) ]
        , p [] [ text (" Sunrise Time      = " ++ mnToHrMn  (risetMns model -1) ++ locTZ model) ]
        , p [] [ text (" Sunset Time       = " ++ mnToHrMn  (risetMns model 1) ++ locTZ model) ]
        , p [] [ text (" Sun Altitude      = " ++ (cutDecNum (90.0 - (solZenith   model (getCentury model)))) ++ "°")]
        , p [] [ text (" Solar Azimuth     = " ++ (cutDecNum (solAzimuth   model (getCentury model))) ++ "°")]
        ]


viewFooter : Model -> Html msg
viewFooter model =
    div [ style "color" "black", style "font-size" "1.0em" ]
        [ p [ style "margin-right" "50%" ]
            [ text
                ("This programm contains the basic functions to be used in calculation of solar positions,"
                    ++ " Sunrise and Sunset times for the given geographic location, date and time."
                )
            ]
        , p [] [ text "The code is written in Elm-programming language version 0.19.0" ]
        , p [] [ text "© J. Lammi 2019" ]
        ]


locTZ mod =
    " UTC +" ++ mod.timezone ++ " h local time"



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


acosDeg =
    \x -> toDeg (acos x)


atan2Deg =
    \x y -> toDeg (atan2 x y)


decNorm360 =
    \arg -> toFloat (remainderBy 360 (floor arg)) + frac arg


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
    (toFloat (gI mod.hour) - 12.0 + toFloat (gI mod.minute) / 60.0) / 24.0


gI element =
    Maybe.withDefault 0 (toInt element)


jdn y m d =
    (1461 * (y + 4800 + (m - 14) // 12))
        // 4
        + (367 * (m - 2 - 12 * ((m - 14) // 12)))
        // 12
        - (3 * ((y + 4900 + (m - 14) // 12) // 100))
        // 4
        + d
        - 32075


getDecVar x =
    Maybe.withDefault 0 (String.toFloat x)


getInputValue laji =
    Maybe.withDefault 0 (toInt laji)


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


zeroFill x =
    if x < 10 then
        "0" ++ String.fromInt x

    else
        String.fromInt x


-- Cut to six decimals format of Float-numbers
cutDecNum : Float -> String
cutDecNum nr =
    let
        snr = String.fromFloat nr
        dotIndex = String.indices "." snr
        dotNr = Maybe.withDefault 0 (List.minimum dotIndex)
        decNrLength = String.length snr
        decimPart = String.slice dotNr decNrLength snr
        cutTo6 = String.left 7 decimPart
        intPart = String.left dotNr snr
    in
        intPart ++ cutTo6



