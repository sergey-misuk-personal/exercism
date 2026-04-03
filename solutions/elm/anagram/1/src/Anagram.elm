module Anagram exposing (detect)


detect : String -> List String -> List String
detect word candidates =
    let
        toLetters : String -> List Char
        toLetters =
            List.sort << String.toList << String.toLower

        wordLetters : List Char
        wordLetters =
            toLetters word

        wordInLower : String
        wordInLower =
            String.toLower word

        differentWord : String -> Bool
        differentWord candidate =
            String.toLower candidate /= wordInLower

        sameLetters : String -> Bool
        sameLetters candidate =
            let
                candidateLetters =
                    toLetters candidate
            in
            candidateLetters == wordLetters
    in
    candidates
        |> List.filter differentWord
        |> List.filter sameLetters
