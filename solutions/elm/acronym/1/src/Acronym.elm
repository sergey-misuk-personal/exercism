module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate =
    let
        firstLetterCapitalized : String -> String
        firstLetterCapitalized =
            String.slice 0 1 >> String.toUpper

        splitByHyphen : String -> List String
        splitByHyphen =
            String.split "-"
    in
    String.words
        >> List.concatMap splitByHyphen
        >> List.map firstLetterCapitalized
        >> String.concat
