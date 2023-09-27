module Main exposing (main)

import Browser
import Html exposing (Html, div, input, p, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , chompWhile
        , getChompedString
        , problem
        , spaces
        , succeed
        )



-- MODEL


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "60 12 N 24 51 E" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }



-- VIEW


getLocation : String -> String
getLocation inputS =
    let
        r2 : ( Float, Float )
        r2 =
            parseLocation inputS
    in
    "Latitude "
        ++ String.fromFloat (Tuple.first r2)
        ++ "° ; "
        ++ " Longitude "
        ++ String.fromFloat (Tuple.second r2)
        ++ "°"


view : Model -> Html Msg
view model =
    div [ style "margin" "5%" ]
        [ input [ placeholder "Text to parse", value model.content, onInput Change ] []
        , div [ style "font-size" "2em", style "color" "blue" ]
            [ p [] [ text (getLocation model.content) ]
            ]
        ]


type alias GeoLocation =
    { latdeg : Int
    , latmin : Int
    , londeg : Int
    , lonmin : Int
    }


geoLocation : Parser GeoLocation
geoLocation =
    succeed GeoLocation
        |= intRange 0 90
        |. spaces
        |= intRange 0 60
        |. spaces
        |= intRange 0 180
        |. spaces
        |= intRange 0 60


intRange : Int -> Int -> Parser Int
intRange from to =
    getChompedString (chompWhile Char.isDigit)
        |> andThen (checkRange from to)


checkRange : Int -> Int -> String -> Parser Int
checkRange from to str =
    case String.toInt str of
        Just n ->
            if n >= from && n <= to then
                succeed n

            else
                rangeProblem from to

        Nothing ->
            rangeProblem from to


rangeProblem : Int -> Int -> Parser a
rangeProblem from to =
    problem <|
        String.join " "
            [ "expected a number between"
            , String.fromInt from
            , "and"
            , String.fromInt to
            ]


getFlagLat : String -> Int
getFlagLat ipx =
    if String.contains "S" ipx
    then -1

    else 1

        {-    case String.contains "S" ipx of
        True ->
            -1

        _ ->
            1
        -}

getFlagLon : String -> Int
getFlagLon ipx =
    if String.contains "W" ipx
    then
        -1

    else
        1


prepareInput : String -> String
prepareInput ipx =
    String.replace "S" "" <|
        String.replace "N" "" <|
            String.replace "E" "" <|
                String.replace "W" "" ipx


parseLocation : String -> ( Float, Float )
parseLocation ipLoc =
    let
        ns : Int
        ns =
            getFlagLat ipLoc

        ew : Int
        ew =
            getFlagLon ipLoc

        v1 : String
        v1 =
            prepareInput ipLoc

        zero : GeoLocation
        zero =
            { latdeg = 0, latmin = 0, londeg = 0, lonmin = 0 }

        pz : GeoLocation
        pz =
            Result.withDefault zero (Parser.run geoLocation v1)

        latDecimal : Float
        latDecimal =
            toFloat (ns * (60 * pz.latdeg + pz.latmin)) / 60.0

        lonDecimal : Float
        lonDecimal =
            toFloat (ew * (60 * pz.londeg + pz.lonmin)) / 60.0
    in
    ( latDecimal, lonDecimal )



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- https://ellie-app.com/p32DVYtmTWma1
