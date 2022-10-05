module Main exposing (InputData, Msg, main)

import Browser
import CommonModel exposing (InputData)
import DecimalFormat exposing (cutDec3, cutDec6)
import Html exposing (Html, div, h1, h2, input, p, span, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)
import MnToHrMnSc exposing (mnToHrMn)
import String exposing (fromInt)
import SunHelper
    exposing
        ( atmosRefract
        , civTwlMns
        , decHrToTime
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


main : Program () InputData Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias InputData =
    CommonModel.InputData


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
            , viewCalculated inputData
            , viewFooter
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, style "width" "65px", onInput toMsg ] []


viewValidation : InputData -> Html msg
viewValidation inputData =
    let
        validationResult : String -> String -> Html msg
        validationResult color remark =
            div [ style "color" color ] [ text remark ]
    in
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
        validationResult "blue" "Date maybe Ok"

    else
        validationResult "red" "Date is incorrect!"


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
viewJD mod =
    div [ style "color" "red", style "background-color" "lightblue" ]
        [ text ("JDN " ++ fromInt (getJDN mod))
        , p [] [ text (" JD = " ++ cutDec6 (fJD mod)) ]
        ]


viewCalculated : InputData -> Html msg
viewCalculated inputData =
    let
        viewCalculatedItem : String -> Html msg
        viewCalculatedItem content =
            p [] [ text content ]

        items : List String
        items =
            [ " Sun Declination   = " ++ cutDec6 (sunDeclination inputData) ++ "°"
            , " Day Length        = " ++ decHrToTime (getDayLength inputData)
            , " Civil Twilight    = " ++ mnToHrMn (civTwlMns inputData -1) ++ locTZ inputData
            , morningToNoon inputData
            , " Noon Time         = " ++ mnToHrMn (getNoon inputData) ++ locTZ inputData
            , noonToEvening inputData
            , " Civil Twilight    = " ++ mnToHrMn (civTwlMns inputData 1) ++ locTZ inputData
            , " Solar Azimuth     = " ++ cutDec3 (solAzimuth inputData) ++ "°"
            , " Air refraction    = " ++ cutDec3 (atmosRefract inputData) ++ "°"
            , " Sun Altitude      = " ++ cutDec3 (90.0 - solZenith inputData) ++ "°  without air-refraction"
            , " Sun Altitude      = " ++ cutDec3 (refractCorrectAltitude inputData) ++ "° Corrected with air-refraction"
            ]
    in
    div [ style "color" "green", style "font-size" "1.4em" ]
        (List.map viewCalculatedItem items)


morningToNoon : InputData -> String
morningToNoon mod =
    let
        geoLat : Float
        geoLat =
            getDecVar mod.latitude

        declination : Float
        declination =
            sunDeclination mod
    in
    if declination < 0.0 && geoLat > (90.83 + declination) then
        " Arctic winter, no sunrise"

    else if declination < 0.0 && geoLat < (-89.17 - declination) then
        " Antarctic midsummer, no sunset"

    else if declination > 0.0 && geoLat > (89.17 - declination) then
        " Arctic summer, no sunset"

    else
        let
            dayLength : Float
            dayLength =
                getDayLength mod
        in
        if dayLength > 0.0 && dayLength < 24.0 then
            " Sunrise Time      = " ++ mnToHrMn (sunRise mod) ++ locTZ mod

        else
            let
                a0 : String
                a0 =
                    " Daylength Exception: "
            in
            a0 ++ String.fromFloat dayLength


noonToEvening : InputData -> String
noonToEvening mod =
    let
        geoLat : Float
        geoLat =
            getDecVar mod.latitude

        declination : Float
        declination =
            sunDeclination mod
    in
    if declination < 0.0 && geoLat > (90.8 + declination) then
        " Arctic winter, no Sunrise"

    else if declination < 0.0 && geoLat < -89.2 - declination then
        " No Sunset, Summer in Antarctis"

    else if declination > 0.0 && geoLat > (89.2 - declination) then
        " Polar summer, no sunset"

    else
        " Sunset Time      = " ++ mnToHrMn (sunSet mod) ++ locTZ mod



-- viewFooter : InputData -> Html msg


viewFooter : Html msg
viewFooter =
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


locTZ : InputData -> String
locTZ mod =
    " UTC +" ++ mod.timezone ++ " h local time"
