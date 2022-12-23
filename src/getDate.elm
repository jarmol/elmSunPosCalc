module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events
import Task
import Time


type Date
    = Date
        { year : Int 
        , month : Time.Month
        , day : Int 
        }   


{-| Other states can be added as the applciation grows.
-}
type Model
    = NeedDate
    | WithDate Date


initialModel : Model
initialModel =
    NeedDate
    

type Msg
    = ClickedGetDate
    | GotTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGetDate ->
            ( model
            , Task.perform GotTime Time.now
            )

        GotTime time ->
            ( WithDate (dateFromTime time)
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ case model of
            NeedDate ->
                viewGetDate

            WithDate date ->
                viewDate date
                ]   


viewGetDate : Html Msg 
viewGetDate =
    Html.p []
        [ Html.button
            [ Html.Events.onClick ClickedGetDate ]
            [ Html.text "Get Date" ]
        ]   


viewDate : Date -> Html msg 
viewDate date =
    Html.dl []
        [ Html.dt [] [ Html.text "Date:" ]
        , Html.dd [] [ Html.text (formatDate date) ]
        ]   


{-| Convert the date to a String for rendering
-}
formatDate : Date -> String
formatDate (Date { year, month, day }) =
    formatYear year
        ++ "-" 
        ++ formatMonth month
        

type Msg
    = ClickedGetDate
    | GotTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGetDate ->
            ( model
            , Task.perform GotTime Time.now
            )

        GotTime time ->
            ( WithDate (dateFromTime time)
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ case model of
            NeedDate ->
                viewGetDate

            WithDate date ->
                viewDate date        ++ "-"
                
formatYear : Int -> String
formatYear =
    String.fromInt


formatMonth : Time.Month -> String
formatMonth month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


formatDay : Int -> String
formatDay =
    String.fromInt


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, Cmd.none )
    
    


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


-- Convert a Time.Posix into our Date

dateFromTime : Time.Posix -> Date
dateFromTime posixTime =
    Date
        { year = Time.toYear Time.utc posixTime
        , month = Time.toMonth Time.utc posixTime
        , day = Time.toDay Time.utc posixTime
        }

                
