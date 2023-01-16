module Pages.TableSet exposing (view)

import Html exposing (div, table, td, text, th, tr)
import Html.Attributes as HA
import Page
import UI
import View exposing (View)


headset hname =
    th [] [ text hname ]


valset cell =
    td [] [ text cell ]


view : View msg
view =
    { title = "Create Tables"
    , body =
        UI.layout
            [ div [ HA.style "margin" "5em" ]
                [ table []
                    [ tr []
                        (List.map headset
                            [ "C"
                            , "Si"
                            , "Mn"
                            , "Cr"
                            , "Ni"
                            , "Mo"
                            , "Ti"
                            , "Cu"
                            , "N"
                            ]
                        )
                    , tr [] (List.map valset [ "0.045", "0.45", "1.25", "18.15", "8.55", "0.05", "0.02", "0.15", "0.045" ])
                    ]
                ]
            ]
    }
