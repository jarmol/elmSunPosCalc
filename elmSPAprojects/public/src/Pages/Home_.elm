module Pages.Home_ exposing (view)

import Html as H
import Html.Attributes as HA
import UI
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body =
        UI.layout
            [ H.h1 [] [ H.text "Homepage" ]
            , H.img [ HA.src "/images/josukka.jpg", HA.width 229, HA.height 233 ] []
            , H.img [ HA.src "/images/nov22.png", HA.width 312, HA.height 233 ] []
            ]
    }
