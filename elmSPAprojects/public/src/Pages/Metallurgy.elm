module Pages.Metallurgy exposing (Model, Msg, page)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Gen.Params.Metallurgy exposing (Params)
import Html exposing (Html, a, div, h1, header, input, p, table, td, text, th, tr)
import Html.Attributes as HA
import Html.Events exposing (onInput)
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
    { xC : String
    , xSi : String
    , xMn : String
    , xCr : String
    , xNi : String
    , xMo : String
    , xTi : String
    , xCu : String
    , xN : String
    }


init : Model
init =
    Model "0.045" "0.45" "1.25" "18.15" "8.55" "0.05" "0.02" "0.15" "0.045"



-- UPDATE


type Msg
    = MxC String
    | MxSi String
    | MxMn String
    | MxCr String
    | MxNi String
    | MxMo String
    | MxTi String
    | MxCu String
    | MxN String


update : Msg -> Model -> Model
update msg model =
    case msg of
        MxC xC ->
            { model | xC = xC }

        MxSi xSi ->
            { model | xSi = xSi }

        MxMn xMn ->
            { model | xMn = xMn }

        MxCr xCr ->
            { model | xCr = xCr }

        MxNi xNi ->
            { model | xNi = xNi }

        MxMo xMo ->
            { model | xMo = xMo }

        MxTi xTi ->
            { model | xTi = xTi }

        MxCu xCu ->
            { model | xCu = xCu }

        MxN xN ->
            { model | xN = xN }


headset : String -> Html msg
headset hname =
    th
        [ HA.style "font-size" "14px"
        , HA.style "color" "white"
        , HA.style "background-color" "DarkCyan"
        ]
        [ text hname ]


xnam : List String
xnam =
    [ "%C", "%Si", "%Mn", "%Cr", "%Ni", "%Mo", "%Ti", "%Cu", "%N" ]


xmod : Model -> List String
xmod mod =
    [ mod.xC, mod.xSi, mod.xMn, mod.xCr, mod.xNi, mod.xMo, mod.xTi, mod.xCu, mod.xN ]


xmsg : List (String -> Msg)
xmsg =
    [ MxC, MxSi, MxMn, MxCr, MxNi, MxMo, MxTi, MxCu, MxN ]


trio : Model -> List ( String, String, String -> Msg )
trio mod =
    zip3 xnam (xmod mod) xmsg


essential : String -> String -> (String -> msg) -> Html msg
essential nam x ms =
    let
        s : List (Html.Attribute msg)
        s =
            [ HA.style "padding-left" "10px"
            , HA.style "font-size" "16px"
            , HA.style "width" "60px"
            ]
    in
    td s [ viewInput "string" nam x ms ]


valset : Model -> List (Html Msg)
valset mod =
    List.map (\( nc, cc, mc ) -> essential nc cc mc) (trio mod)



-- VIEW


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> View Msg
view model =
    { title = "Stainless"
    , body =
        UI.layout
            [ header []
                [ h1 [] [ text "Calculator for Austenitic Stainless Steels" ]
                , div [ HA.style "margin" "5em" ]
                    [ table [ HA.style "border" "2px solid black" ]
                        [ tr []
                            (List.map headset
                                xnam
                            )
                        , tr [] (valset model)
                        ]
                    , p [ fs16 ]
                        [ text
                            ("Md30 Temperature (Nohara) = "
                                ++ usDec3 (tempMd30 model)
                                ++ "°C"
                            )
                        ]
                    , p [ fs16 ]
                        [ text
                            ("Ferrite Number FN  = "
                                ++ usDec3 (fnA model)
                                ++ " (austenitic stainless)"
                            )
                        ]
                    , p [ fs16 ]
                        [ text
                            ("Pitting Corrosion Resistance (PRE) = "
                                ++ usDec3 (eqPittingResistance model)
                            )
                        ]
                    ]
                ]
            ]
    }


fs16 : Html.Attribute msg
fs16 =
    HA.style "font-size" "16px"


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ HA.type_ t, HA.placeholder p, HA.value v, HA.style "width" "60px", onInput toMsg ] []


zip : List a -> List b -> List ( a, b )
zip a b =
    List.map2 (\x y -> ( x, y )) a b


zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 a b c =
    List.map3 (\x y z -> ( x, y, z )) a b c


tupleprod : ( number, number ) -> number
tupleprod ( c, x ) =
    c * x


sumOfProducts : List number -> List number -> number
sumOfProducts alist blist =
    zip alist blist
        |> List.map tupleprod
        |> List.sum


xval : String -> Float
xval s =
    Maybe.withDefault 0.0 (String.toFloat s)


tempMd30 : Model -> Float
tempMd30 mod =
    let
        coefficients : List Float
        coefficients =
            [ 462.0, 9.2, 8.1, 13.7, 29.0, 18.5, 29.0, 462.0 ]

        elements : List String
        elements =
            [ mod.xC, mod.xSi, mod.xMn, mod.xCr, mod.xNi, mod.xMo, mod.xCu, mod.xN ]

        composition : List Float
        composition =
            List.map xval elements
    in
    551.0 - sumOfProducts coefficients composition



-- DELTA FERRITE NUMBER FN


fnA : Model -> Float
fnA mod =
    let
        crEkv : Float
        crEkv =
            xval mod.xCr
                + 1.5
                * xval mod.xSi
                + xval mod.xMo
                + 2.0
                * xval mod.xTi

        niEkv : Float
        niEkv =
            xval mod.xNi
                + 0.5
                * xval mod.xMn
                + 30.0
                * xval mod.xC
                + 30.0
                * xval mod.xN
                + 0.5
                * xval mod.xCu

        preFNA : Float
        preFNA =
            3.34 * crEkv - 2.46 * niEkv - 28.6
    in
    if preFNA < 6.0 then
        preFNA

    else if preFNA < 12.0 then
        4.44 * crEkv - 3.39 * niEkv - 38.4

    else
        4.06 * crEkv - 3.23 * niEkv - 32.2


eqPittingResistance : Model -> Float
eqPittingResistance mod =
    let
        coefficients : List Float
        coefficients =
            [ 1.0, 3.3, 20.0 ]

        elements : List String
        elements =
            [ mod.xCr, mod.xMo, mod.xN ]

        composition : List Float
        composition =
            List.map xval elements
    in
    sumOfProducts coefficients composition



-- DECIMAL NUMBERS FORMAT


sharesLocale : FormatNumber.Locales.Locale
sharesLocale =
    { usLocale
        | decimals = Exact 6
        , negativePrefix = "-"
        , positivePrefix = " "
    }


sharesLocaleUS : FormatNumber.Locales.Locale
sharesLocaleUS =
    { sharesLocale
        | decimals = Exact 3
    }


usDec3 : Float -> String
usDec3 x =
    format sharesLocaleUS x
