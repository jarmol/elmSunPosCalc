module Pages.Listprimes exposing (page)

import Html as H
import Html.Attributes as HA
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI exposing (layout)
import View exposing (View)


blist : List Int
blist =
    sieve 200000 202201


numbers : Int
numbers =
    List.length blist


primes : String
primes =
    List.map String.fromInt blist
        |> List.reverse
        |> List.map (\el -> " " ++ el)
        |> List.foldl (++) " "


page : Shared.Model -> Request -> Page
page shared req =
    Page.static
        { view = view
        }


view : View msg
view =
    { title = "List primes"
    , body =
        UI.layout
            [ H.h1 [] [ H.text "Primes" ]
            , H.div [ HA.style "color" "blue", HA.style "padding" "10px" ]
                [ H.text primes
                , H.p [] [ H.text ("Found " ++ String.fromInt numbers ++ " primes") ]
                ]
            ]
    }


sieve : Int -> Int -> List Int
sieve min limit =
    let
        numbrs : List Int
        numbrs =
            List.range 2 limit

        last : Int
        last =
            limit
                |> toFloat
                |> sqrt
                |> round

        isMultiple : Int -> Int -> Bool
        isMultiple n m =
            n /= m && modBy m n == 0
    in
    List.range 2 last
        |> List.foldl
            (\current result ->
                List.filter
                    (\elem -> not (isMultiple elem current))
                    result
            )
            numbrs
        |> List.filter (\el -> el >= min)
