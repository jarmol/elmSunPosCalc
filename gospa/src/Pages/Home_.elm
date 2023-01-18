module Pages.Home_ exposing (view)

import View exposing (View)
import Html as H
import Html.Attributes as HA


view : View msg
view =
    { title = "Home SPA"
    , body = [ H.h1 [HA.style "color" "blue", HA.style "font-family" "Helvetica neue, sans-serif"]
             [H.text "How to do deployment in elm-spa"]
    , H.h2 [HA.style "font-family" "Arial, sans-serif"] [H.text "Guide"]
    , H.p [] [ H.a [ HA.href "https://www.elm-spa.dev/examples" ]
                       [ H.text "Examples  in elm-spa" ] ]
    , H.p [] [ H.a [ HA.href "https://tkuriyama.github.io/general/2021/05/27/apache-rewrite-rule.html" ] [ H.text "Apache Rewrite Rules for Elm SPA" ] ]
             ]
    }
