module Main exposing (..)

import Browser
import DecimalFormat exposing (cutDec3, cutDec6)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import MnToHrMnSc exposing (mnToHrMn)
import String exposing (fromFloat, fromInt)
import SunHelper
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

-- This version has results very near to official sunrise, sunset times
-- and daylength.

-- File saved in https://ellie-app.com/7drvTVwxKGVa1
-- and https://ellie-app.com/7drL34ytZTWa1
-- Calculates some parametres of the solar position
-- This code is compatible with Elm 0.19.1
-- © 2022  Jarmo Lammi
-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Date = String
type alias Time = String
type alias GeoLocation = String


type alias Model =
    { year : Date
    , month : Date
    , day : Date
    , hour : Time
    , minute : Time
    , second : Time
    , latitude  : GeoLocation
    , longitude : GeoLocation
    , timezone : String
    }


init : Model
init =
    Model "2022" "12" "1" "10" "12" "16" "65.85" "24.18" "2"



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



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin-left" "10%", style "margin-right" "20%" ]
        [ h1 [] [ text "Sun Position Calculator" ]
        , span [ style "background-color" "blue", style "color" "white" ]
            [ text "  Year "
            , viewInput "number" "Give year" model.year Year
            , text "  Month    "
            , viewInput "number" "Month" model.month Month
            , text "  Day   "
            , viewInput "number" "Day" model.day Daynumber
            , p [] []
            , text "  Hours UTC "
            , viewInput "number" "Hour" model.hour Hour
            , text "  Minutes  "
            , viewInput "number" "Minute" model.minute Minute
            , text "  Seconds  "
            , viewInput "number" "Second" model.second Second
            , h2 [ style "color" "black" ] [ text "Location " ]
            , text " Latitude "
            , viewInput "text" "Latitude" model.latitude Latitude
            , text "  Longitude "
            , viewInput "text" "Longitude" model.longitude Longitude
            , text "  Timezone  "
            , viewInput "number" "Timezone" model.timezone Timezone
            , viewDateValidation model
            , viewTimeValidation model
            , viewResults model
            , viewJD model
            , viewDeclination model
            , viewFooter model
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, style "width" "65px", onInput toMsg ] []


viewDateValidation : Model -> Html msg
viewDateValidation model =
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



viewTimeValidation : Model -> Html msg 
viewTimeValidation model =
    if  
        getInputValue model.hour
            >= 0 
            && getInputValue model.hour
            < 24
            && getInputValue model.minute
            >= 0 
            && getInputValue model.minute
            < 60
            && getInputValue model.second
            >= 0
            && getInputValue model.second
            < 60
    then
        div [ style "color" "blue" ] [ text " Time entry OK" ]

    else
        div [ style "color" "red" ] [ text "Incorrect time" ]


viewResults : Model -> Html msg
viewResults model =
    div [ style "color" "orange"
        , style "background-color" "black"
        , style "padding" "15px" ]
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
        , p []
            [ text
                (" Hours UTC "
                    ++ model.hour
                    ++ " Minutes "
                    ++ model.minute
                    ++ " Seconds "
                    ++ model.second
                )
            ]
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
    div [ style "padding" "10px", style "background-color" "blue" ]
        [ p [] [ text ("Julian Day Number JDN " ++ fromInt (getJDN model)) ]
        , text ("Julian Date JD = " ++ cutDec6 (fJD model))
        ]


viewDeclination : Model -> Html msg
viewDeclination model =
    div [ style "color" "black", style "font-size" "1.4em" ]
        [ p [] [ text (" Sun Declination   = " ++ cutDec3 (sunDeclination model) ++ "°") ]
        , p [] [ text (" Day Length        = " ++ mnToHrMn (60 * getDayLength model)) ]
        , p [] [ text (" Civil Twilight    = " ++ mnToHrMn (civTwlMns model -1) ++ locTZ model) ]
        , p [] [ text (morningToNoon model) ]
        , p [] [ text (" Noon Time         = " ++ mnToHrMn (getNoon model) ++ locTZ model) ]
        , p [] [ text (noonToEvening model) ]
        , p [] [ text (" Civil Twilight    = " ++ mnToHrMn (civTwlMns model 1) ++ locTZ model) ]
        , p [] [ text (" Solar Azimuth     = " ++ cutDec3 (solAzimuth model) ++ "°") ]
        , p [] [ text (" Air refraction    = " ++ cutDec3 (atmosRefract model) ++ "°") ]
        , p []
            [ text
                (" Sun Altitude      = "
                    ++ cutDec3 (90.0 - solZenith model)
                    ++ "°  without air-refraction"
                )
            ]
        , p []
            [ text
                (" Sun Altitude      = "
                    ++ cutDec3 (refractCorrectAltitude model)
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


viewFooter : Model -> Html msg
viewFooter model =
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


locTZ : Model -> String
locTZ mod =
    " UTC +" ++ mod.timezone ++ " h local time"
