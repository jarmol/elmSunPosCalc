module Pages.About exposing (view)

import Gen.Params.About exposing (Params)
import Html exposing (a, h1, p, text)
import Html.Attributes as Attr
import Request
import Shared
import UI exposing (layout)
import View exposing (View)



--import Page exposing (Page)
{-
   page : Shared.Model -> Request.With Params -> Page
   page shared req =
       Page.static
           { view = view
           }
-}


view : View msg
view =
    { title = "About"
    , body =
        UI.layout
            [ h1 [] [ text "About" ]
            , p [] [ text "This was made with elm-spa." ]
            , p [] [ a [ Attr.href "https://www.elm-spa.dev/examples/02-pages" ] [ text "Examples, pages & routing in  elm-spa" ] ]
            ]
    }
