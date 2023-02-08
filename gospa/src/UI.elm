module UI exposing (layout)

import Gen.Route as Route exposing (Route)
import Html exposing (Html, a, div, header, li, p, text, ul)
import Html.Attributes as HA


layout : List (Html msg) -> List (Html msg)
layout children =
    let
        viewLink : String -> Route -> Html msg
        viewLink label route =
            a [ HA.href (Route.toHref route) ] [ text label ]
    in
    [ div [ HA.class "container" ]
        [ header []
            [ ul []
                [ li [] [ viewLink "Home" Route.Home_ ]
                , li [] [ viewLink "About" Route.About ]
                , li [] [ viewLink "BMI" Route.BodyMassIndex ]
                , li [] [ viewLink "Angles" Route.Converter ]
                , li [] [ viewLink "Primes" Route.Listprimes ]
                , li [] [ viewLink "Pythagorean" Route.Pythagorean ]
                , li [] [ viewLink "HTML-tables" Route.TableSet ]
                , li [] [ viewLink "Stainless" Route.Metallurgy ]
                , li [] [ viewLink "Sun-calculator" Route.SunCalculator ]
                , li [] [ viewLink "Buttons" Route.Buttons ]
                , li [] [ viewLink "Settings.Account" Route.Settings__Account ]
--              , li [] [ viewLink "People.Name_" Route.People ]
                ]
            ]
        ]
    , Html.main_ [] children
    , p [ HA.style "margin-top" "10em" ] []
    , p [] []
    , text "This page was built with "
    , a [ HA.href "https://elm-lang.org/" ] [ text "Elm " ]
    , text " programming language and "
    , a [ HA.href "https://www.elm-spa.dev/" ] [ text "elm-spa" ]
    , text " for single page apps"
    , p [] [ text "© 2023 J. Lammi" ]
    ]
