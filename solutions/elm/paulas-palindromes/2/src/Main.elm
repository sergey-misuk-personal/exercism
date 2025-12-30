module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)



-- When you have finished this exercise, this file will be an Elm program.
--
-- You can copy this file to Ellie (an online Elm sandbox - https://ellie-app.com/)
-- to see it running and interact with it.
--
-- If you are solving the exercise locally (and not through the exercism online editor),
-- you can additionally run it locally:
-- - `rm src/Browser.elm` (delete a fake file that we need for the online editor),
-- - `elm install elm/browser` (install the real version of the fake)
-- - `npx elm-watch@beta hot` (compile the code, hot reload and serve)
-- - Then go to the url in the output to view the application (localhost://53529 or similar)
-- MAIN


main : Html Msg
main =
    Browser.sandbox { init = init, view = view, update = update }



-- MODEL


type alias Model =
    { content : String }


init : Model
init =
    { content = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change content ->
            { model | content = content }



-- VIEW


isPalindrome : String -> Bool
isPalindrome content =
    content == String.reverse content


view : Model -> Html Msg
view model =
    let
        message =
            case isPalindrome model.content of
                True ->
                    "This is a palindrome"

                False ->
                    "Not a palindrome"
    in
    div []
        [ input
            [ placeholder "Text to reverse"
            , value model.content
            , onInput Change
            ]
            []
        , div [] [ text message ]
        ]
