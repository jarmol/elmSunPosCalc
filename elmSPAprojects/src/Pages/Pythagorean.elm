module Pages.Pythagorean exposing (view)

import Html exposing (a, div, h1, p, text)
import Html.Attributes as HA
import Page exposing (Page)
import UI exposing (layout)
import View exposing (View)


view : View msg
view =
    { title = "Pythagorean Triples"
    , body =
        UI.layout
            [ div
                [ HA.style "font-size" "120%"
                , HA.style "margin-left" "12px"
                , HA.style "background-color" "#88ffe8"
                ]
                [ h1 [] [ text "Pythagorean triples" ]
                , p [] [ text listTuplesString ]
                , p [] [ text "Longer output" ]
                , p [] [ text (addEquation t1) ]
                , p [] [ text (addEquation t2) ]
                , p [] [ text (addEquation t3) ]
                , p [] [ text (addEquation t4) ]
                , p [] [ text (addEquation t5) ]
                , p [] [ text (addEquation t6) ]
                , p [] [ text (addEquation t7) ]
                , p [] [ text (addEquation t8) ]
                , p [] []
                , text "Read more about "
                , a [ HA.href "https://en.wikipedia.org/wiki/Pythagorean_triple" ] [ text "Pythagorean triples" ]
                ]
            ]
    }


nextTriple : ( Int, Int, Int ) -> ( Int, Int, Int )
nextTriple ( a, b, c ) =
    let
        anext =
            a + 2

        bnext =
            2 * a + b + 2

        cnext =
            2 * a + b + 3
    in
    ( anext, bnext, cnext )


printTriple ( a, b, c ) =
    let
        sa =
            String.fromInt a

        sb =
            String.fromInt b

        sc =
            String.fromInt c
    in
    " ( " ++ sa ++ ", " ++ sb ++ ", " ++ sc ++ " ) "


addEquation ( a, b, c ) =
    let
        sa =
            String.fromInt a

        sb =
            String.fromInt b

        sc =
            String.fromInt c

        sd =
            String.fromInt (c * c)
    in
    printTriple ( a, b, c )
        ++ " --> "
        ++ sa
        ++ "² + "
        ++ sb
        ++ "² = "
        ++ sc
        ++ "²"
        ++ " = "
        ++ sd


t1 : ( Int, Int, Int )
t1 =
    ( 3, 4, 5 )


t2 : ( Int, Int, Int )
t2 =
    nextTriple t1


t3 : ( Int, Int, Int )
t3 =
    nextTriple t2


t4 : ( Int, Int, Int )
t4 =
    nextTriple t3


t5 : ( Int, Int, Int )
t5 =
    nextTriple t4


t6 : ( Int, Int, Int )
t6 =
    nextTriple t5


t7 : ( Int, Int, Int )
t7 =
    nextTriple t6


t8 : ( Int, Int, Int )
t8 =
    nextTriple t7


listTuples nMax =
    List.range 1 nMax
        |> List.map
            (\n ->
                ( 2 * n + 1
                , 2 * n * (n + 1)
                , 2 * n * (n + 1) + 1
                )
            )


listTuplesString =
    List.map printTriple (listTuples 24)
        |> List.reverse
        |> List.foldl (++) " "
