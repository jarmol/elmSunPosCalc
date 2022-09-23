module WorkSuncalculator exposing (..)

import Browser
import DecimalFormat exposing (cutDec3, cutDec6)
import GregorJDN exposing (jdateGr, jdnGr)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import MnToHrMnSc exposing (mnToHrMn)
import String exposing (fromFloat, fromInt, toInt)
import WorkSunHelper
    exposing
        ( atmosRefract
        , civTwlMns
        , fJD
        , getDayLength
        , getDecVar
        , getInputValue
        , getJDN
        , getNoon
        , refractCorrectAltitude
        , solAzimuth
        , solZenith
        , sunDeclination
        , sunRise
        , sunSet
        )



-- Initial data set to Autumn equinox date 2022-9-23
-- and time 01:04:27 UTC
-- © 2022  Jarmo Lammi
-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias InputData =
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


init : InputData
init =
    InputData "2022" "09" "23" "1" "04" "27" "60.0" "25.0" "3"



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


update : Msg -> InputData -> InputData
update msg inputData =
    case msg of
        Year year ->
            { inputData | year = year }

        Month month ->
            { inputData | month = month }

        Daynumber day ->
            { inputData | day = day }

        Hour hour ->
            { inputData | hour = hour }

        Minute minute ->
            { inputData | minute = minute }

        Second second ->
            { inputData | second = second }

        Latitude latitude ->
            { inputData | latitude = latitude }

        Longitude longitude ->
            { inputData | longitude = longitude }

        Timezone timezone ->
            { inputData | timezone = timezone }



-- VIEW


view : InputData -> Html Msg
view inputData =
    div [ style "margin-left" "10%", style "margin-right" "20%" ]
        [ h1 [] [ text "Sun Position Calculator" ]
        , span [ style "background-color" "blue", style "color" "white" ]
            [ text "  Year "
            , viewInput "number" "Give year" inputData.year Year
            , text "  Month    "
            , viewInput "number" "Month" inputData.month Month
            , text "  Day   "
            , viewInput "number" "Day" inputData.day Daynumber
            , p [] []
            , text "  Hours UTC "
            , viewInput "number" "Hour" inputData.hour Hour
            , text "  Minutes  "
            , viewInput "number" "Minute" inputData.minute Minute
            , text "  Seconds  "
            , viewInput "number" "Second" inputData.second Second
            , h2 [ style "color" "black" ] [ text "Location " ]
            , text " Latitude "
            , viewInput "text" "Latitude" inputData.latitude Latitude
            , text "  Longitude "
            , viewInput "text" "Longitude" inputData.longitude Longitude
            , text "  Timezone  "
            , viewInput "number" "Timezone" inputData.timezone Timezone
            , viewValidation inputData
            , viewResults inputData
            , viewJD inputData
            , viewDeclination inputData
            , viewFooter inputData
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, style "width" "65px", onInput toMsg ] []


viewValidation : InputData -> Html msg
viewValidation inputData =
    if
        getInputValue inputData.month
            > 0
            && getInputValue inputData.month
            < 13
            && getInputValue inputData.day
            > 0
            && getInputValue inputData.day
            < 32
    then
        div [ style "color" "blue" ] [ text "Date entry OK" ]

    else
        div [ style "color" "red" ] [ text "Incorrect month or day" ]


viewResults : InputData -> Html msg
viewResults inputData =
    div [ style "color" "orange", style "background-color" "black" ]
        [ h2 [] [ text "DATE AND LOCATION USED BELOW" ]
        , p []
            [ text
                ("Year  "
                    ++ inputData.year
                    ++ ", month    "
                    ++ inputData.month
                    ++ " and day  "
                    ++ inputData.day
                )
            ]
        , p []
            [ text
                (" Hours UTC "
                    ++ inputData.hour
                    ++ " Minutes "
                    ++ inputData.minute
                    ++ " Seconds "
                    ++ inputData.second
                )
            ]
        , p []
            [ text
                ("  Latitude "
                    ++ inputData.latitude
                    ++ "°  Longitude "
                    ++ inputData.longitude
                    ++ "°  Timezone "
                    ++ inputData.timezone
                    ++ " h"
                )
            ]
        ]


viewJD : InputData -> Html msg
viewJD inputdata =
    div [ style "color" "red", style "background-color" "lightblue" ]
        [ p [] [ text ("JDN " ++ fromInt (getJDN inputdata)) ]
        , text (" JD = " ++ cutDec6 (fJD inputdata))
        ]


viewDeclination : InputData -> Html msg
viewDeclination inputdata =
    div [ style "color" "green", style "font-size" "1.4em"]
        [ p [] [ text (" Sun Declination   = " ++ cutDec6 (sunDeclination inputdata) ++ "°") ]
        , p [] [ text (" Day Length        = " ++ mnToHrMn (60 * getDayLength inputdata)) ]
        , p [] [ text (" Civil Twilight    = " ++ mnToHrMn (civTwlMns inputdata -1) ++ locTZ inputdata) ]
        , p [] [ text (morningToNoon inputdata) ]
        , p [] [ text (" Noon Time         = " ++ mnToHrMn (getNoon inputdata) ++ locTZ inputdata) ]
        , p [] [ text (noonToEvening inputdata) ]
        , p [] [ text (" Civil Twilight    = " ++ mnToHrMn (civTwlMns inputdata 1) ++ locTZ inputdata) ]
        , p [] [ text (" Solar Azimuth     = " ++ cutDec3 (solAzimuth inputdata) ++ "°") ]
        , p [] [ text (" Air refraction    = " ++ cutDec3 (atmosRefract inputdata) ++ "°") ]
        , p []
            [ text
                (" Sun Altitude      = "
                    ++ cutDec3 (90.0 - solZenith inputdata)
                    ++ "°  without air-refraction"
                )
            ]
        , p []
            [ text
                (" Sun Altitude      = "
                    ++ cutDec3 (refractCorrectAltitude inputdata)
                    ++ "° Corrected with air-refraction"
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


viewFooter : InputData -> Html msg
viewFooter inputdata =
    div [ style "color" "black", style "font-size" "1.0em" ]
        [ p [ style "margin-right" "50%" ]
            [ text
                """
                 This program contains the basic functions
                 to be used in calculation of solar positions,
                 Sunrise and Sunset times for the given
                 geographic location, date and time.
                 """
            ]
        , p [ style "color" "blue" ]
            [ text "The code is written in Elm-programming language version 0.19.1" ]
        , p [] [ text "© J. Lammi 2019 - 2022" ]
        ]


locTZ mod =
    " UTC +" ++ mod.timezone ++ " h local time"
