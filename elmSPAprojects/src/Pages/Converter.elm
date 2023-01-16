module Pages.Converter exposing (view)

import DecimalFormat exposing (cutDec2, cutDec6)
import Html as H
import Html.Attributes as HA
import Page exposing (Page)
import UI exposing (layout)
import View exposing (View)


radToDeg : Float -> Float
radToDeg xrad =
    180 * xrad / pi


showThat divisor =
    H.p [ HA.style "color" "blue" ]
        [ H.text
            ("π/"
                ++ String.fromFloat divisor
                ++ " rad = "
                ++ cutDec2 (radToDeg (pi / divisor))
                ++ "°"
            )
        ]


view : View msg
view =
    { title = "π-kalkulaattori"
    , body =
        UI.layout
            [ H.h1 [ HA.style "margin-top" "5%" ]
                [ H.text "Radians to Grades" ]
            , H.div
                [ HA.style "font-size" "120%"
                , HA.style "margin-left" "5%"
                ]
                (List.map showThat [ 15, 9, 7.5, 6, 4, 3, 2, 1 ])
            ]
    }
