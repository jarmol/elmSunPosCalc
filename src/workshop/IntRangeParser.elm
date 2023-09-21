module Main exposing (main)

-- Parsing "latdeg,latmin londeg,lonmin" -> {latdeg,latmin,londeg,lonmin} 

import Parser exposing (run,
                        getChompedString,
                        chompWhile,
                        andThen,
                        succeed,
                        problem,
                        symbol,
                        Parser,
                        (|=),
                        (|.),
                        spaces)
 
import Html exposing (text)

type alias GeoLocation =
    { latdeg : Int
    , latmin : Int
    , londeg : Int
    , lonmin : Int
    }


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
rangeProblem from to = problem <|
                String.join " "
                [ "expected a number between"
                , String.fromInt from
                , "and"
                , String.fromInt to
                ]


geoLocation : Parser GeoLocation
geoLocation =
    succeed GeoLocation
        |= intRange 0 90
        |. symbol ","
        |= intRange 0 60
        |. spaces
        |= intRange 0 180
        |. symbol ","
        |= intRange 0 60


main =
   let s = "60,12 24,52"
   in
       text <| "Parse " ++ s ++ " -> " ++ Debug.toString (run geoLocation s)

-- See this in https://ellie-app.com/nYp2MsSvFcTa1
-- More parser examples here
-- https://discourse.elm-lang.org/t/how-to-use-elm-parser-to-parse-ints-followed-by-a-period-like-in-ip-addresses/2829/3
