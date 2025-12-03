module WordCount exposing (wordCount)

import Dict exposing (Dict)
import Set exposing (Set)


punctuation : Set Char
punctuation =
    Set.fromList [ '!', '$', '%', '&', ',', '.', ':', '@', '^' ]


space : Char
space =
    ' '


isNotEmpty : String -> Bool
isNotEmpty =
    not << String.isEmpty


words : String -> List String
words sentence =
    let
        replacePunctuation : Char -> Char
        replacePunctuation c =
            if Set.member c punctuation then
                space

            else
                c

        removeApostrophes w =
            if String.startsWith "'" w then
                removeApostrophes <| String.dropLeft 1 w

            else if String.endsWith "'" w then
                removeApostrophes <| String.dropRight 1 w

            else
                w
    in
    sentence
        |> String.toList
        |> List.map replacePunctuation
        |> String.fromList
        |> String.words
        |> List.map removeApostrophes
        |> List.filter isNotEmpty


countWord : String -> Dict String Int -> Dict String Int
countWord word counterDict =
    let
        updateCounter : Maybe Int -> Maybe Int
        updateCounter counter =
            case counter of
                Just c ->
                    Just <| c + 1

                Nothing ->
                    Just 1
    in
    Dict.update word updateCounter counterDict


wordCount : String -> Dict String Int
wordCount sentence =
    sentence
        |> words
        |> List.map String.toLower
        |> List.foldl countWord Dict.empty
