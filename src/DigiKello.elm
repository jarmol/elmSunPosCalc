module DigiKello exposing (main)

-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

import Browser
import GregorJDN exposing (weekday)
import Html exposing (div, h1, p, text)
import Html.Attributes exposing (style)
import Task
import Time exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 2000 Tick


monthName : Time.Month -> String
monthName month_ =
    case month_ of
        Jan ->
            "tammi"

        Feb ->
            "helmi"

        Mar ->
            "maalis"

        Apr ->
            "huhti"

        May ->
            "touko"

        Jun ->
            "kesä"

        Jul ->
            "heinä"

        Aug ->
            "elo"

        Sep ->
            "syys"

        Oct ->
            "loka"

        Nov ->
            "marras"

        Dec ->
            "joulu"


wdayName : Time.Weekday -> String
wdayName weekDay_ =
    case weekDay_ of
        Mon ->
            "Maanantai"

        Tue ->
            "Tiistai"

        Wed ->
            "Keskiviikko"

        Thu ->
            "Torstai"

        Fri ->
            "Perjantai"

        Sat ->
            "Lauantai"

        Sun ->
            "Sunday"


preZero : Int -> String
preZero minTen =
    if minTen < 10 then
        "0" ++ String.fromInt minTen

    else
        String.fromInt minTen



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        year =
            String.fromInt (Time.toYear model.zone model.time)

        month =
            monthName (Time.toMonth model.zone model.time)

        day =
            String.fromInt (Time.toDay model.zone model.time)

        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            preZero <| Time.toMinute model.zone model.time

        second =
            preZero <| Time.toSecond model.zone model.time

        weekday =
            wdayName <| Time.toWeekday model.zone model.time
    in
    div
        [ style "margin" "5%"
        , style "background-color" "black"
        , style "color" "yellow"
        ]
        [ p [ style "font-size" "1.5em" ]
            [ text
                ("Pvm. "
                    ++ day
                    ++ ". "
                    ++ month
                    ++ "kuuta vuonna "
                    ++ year
                )
            ]
        , p [ style "font-size" "1.5em" ] [ text weekday ]
        , h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        ]
