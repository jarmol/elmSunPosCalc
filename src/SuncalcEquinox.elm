module SuncalcEquinox exposing (Model, Msg(..), main)

import Browser
import DecimalFormat exposing (cutDec3, cutDec6)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import MnToHrMnSc exposing (mnToHrMn)
import String exposing (fromInt)
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



-- Initial data set to Autumn equinox date 2022-9-23
-- and time 01:04:27 UTC
-- © 2022  Jarmo Lammi
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
    , second : String
    , latitude : String
    , longitude : String
    , timezone : String
    }


init : Model
init =
    Model "2022" "09" "23" "1" "04" "27" "60.0" "25.0" "3"



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
            , viewValidation model
            , viewResults model
            , viewJD model
            , viewDeclination model
            , viewFooter
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, style "width" "65px", onInput toMsg ] []


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
    div [ style "color" "red", style "background-color" "lightblue" ]
        [ p [] [ text ("JDN " ++ fromInt (getJDN model)) ]
        , text (" JD = " ++ cutDec6 (fJD model))
        ]


viewDeclination : Model -> Html msg
viewDeclination model =
    div [ style "color" "red", style "font-size" "1.4em" ]
        [ p [] [ text (" Sun Declination   = " ++ cutDec6 (sunDeclination model) ++ "°") ]
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
        geoLat =
            getDecVar mod.latitude

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
            dayLength =
                getDayLength mod
        in
        if dayLength > 0.0 && dayLength < 24.0 then
            " Sunrise Time      = " ++ mnToHrMn (sunRise mod) ++ locTZ mod

        else
            let
                a0 =
                    " Daylength Exception: "
            in
            a0 ++ String.fromFloat dayLength


noonToEvening mod =
    let
        geoLat =
            getDecVar mod.latitude

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


locTZ mod =
    " UTC +" ++ mod.timezone ++ " h local time"
