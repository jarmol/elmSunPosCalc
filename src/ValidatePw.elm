module ValidatePw exposing (main)

import Browser
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)



-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--
-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin" "5%" ]
        [ h1 [] [ text "Create better passwords!" ]
        , viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.password /= model.passwordAgain then
        div [ style "color" "red" ] [ text "incorrect!" ]

    else if String.length model.password < 8 then
        div [ style "color" "red" ] [ text "Password too short!" ]

    else if pwValidHelper 'A' 'Z' model.password == 0 then
        div [ style "color" "red" ] [ text "No upper case found!" ]

    else if pwValidHelper 'a' 'z' model.password == 0 then
        div [ style "color" "red" ] [ text "No lower case found!" ]

    else if pwValidHelper '0' '9' model.password == 0 then
        div [ style "color" "red" ] [ text "No numbers found!" ]

    else
        div [ style "color" "green" ] [ text "OK" ]


pwValidHelper : Char -> Char -> String -> Int
pwValidHelper lower upper pw =
    String.length (String.filter (\c -> (c >= lower) && (c <= upper)) pw )
