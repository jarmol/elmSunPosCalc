module Pages.Buttons exposing (Model, Msg, page)

import Gen.Params.Metallurgy exposing (Params)
import Html exposing (Html, button, div, h1, header, text)
import Html.Events exposing (onClick)
import Page
import Request
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement ->
            model - 1

        Increment ->
            model + 1



-- VIEW


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> View Msg
view model =
    { title = "Buttons"
    , body =
        UI.layout
            [ header []
                [ h1 [] [ text "Buttons" ]
                , div []
                    [ button [ onClick Increment ] [ text "+" ]
                    , div [] [ text (String.fromInt model) ]
                    , button [ onClick Decrement ] [ text "-" ]
                    ]
                ]
            ]
    }
