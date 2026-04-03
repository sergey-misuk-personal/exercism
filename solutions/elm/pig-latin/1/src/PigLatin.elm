module PigLatin exposing (..)

import Set exposing (Set)


vowels : Set Char
vowels =
    Set.fromList <| String.toList "aeiou"


type alias Predicate a =
    a -> Bool


type alias Translator a =
    a -> a


type alias Rule =
    ( Predicate String, Translator String )


isVowel : Char -> Bool
isVowel l =
    Set.member l vowels


isConsonant : Char -> Bool
isConsonant =
    not << isVowel


firstLetter : String -> Maybe Char
firstLetter word =
    word
        |> String.toList
        |> List.head


startsWithLetter : Predicate Char -> String -> Bool
startsWithLetter letterPredicate word =
    case firstLetter word of
        Just l ->
            letterPredicate l

        Nothing ->
            False


startsWithVowel : Predicate String
startsWithVowel =
    startsWithLetter isVowel


startsWithConsonant : Predicate String
startsWithConsonant =
    startsWithLetter isConsonant


takeWhile : Predicate a -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs

            else
                []


splitOnceBy : Predicate a -> List a -> ( List a, List a )
splitOnceBy predicate list =
    let
        first =
            takeWhile predicate list

        second =
            List.drop (List.length first) list
    in
    ( first, second )


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: xs ->
            last xs


notYConsonant : Predicate Char
notYConsonant l =
    isConsonant l && l /= 'y'



-- rule implementations


firstRulePredicate : Predicate String
firstRulePredicate word =
    startsWithVowel word || String.startsWith "xr" word || String.startsWith "yt" word


firstRuleTranslator : Translator String
firstRuleTranslator word =
    String.concat [ word, "ay" ]


secondRulePredicate : Predicate String
secondRulePredicate =
    startsWithConsonant


secondRuleTranslator : Translator String
secondRuleTranslator word =
    let
        ( prefix, rest ) =
            splitOnceBy isConsonant <| String.toList word
    in
    String.concat [ String.fromList rest, String.fromList prefix, "ay" ]


thirdRulePredicate : Predicate String
thirdRulePredicate word =
    let
        ( consonants, rest ) =
            splitOnceBy isConsonant <| String.toList word
    in
    case ( last consonants, List.head rest ) of
        ( Just 'q', Just 'u' ) ->
            True

        _ ->
            False


thirdRuleTranslator : Translator String
thirdRuleTranslator word =
    case String.indices "qu" word of
        x :: _ ->
            let
                prefixLength =
                    x + 2

                prefix =
                    String.slice 0 prefixLength word

                rest =
                    String.slice prefixLength (String.length word) word
            in
            String.concat [ rest, prefix, "ay" ]

        _ ->
            word


fourthRulePredicate : Predicate String
fourthRulePredicate word =
    let
        ( consonants, rest ) =
            splitOnceBy notYConsonant <| String.toList word
    in
    if List.length consonants > 0 then
        List.head rest == Just 'y'

    else
        False


fourthRuleTranslator : Translator String
fourthRuleTranslator word =
    case String.indices "y" word of
        x :: _ ->
            let
                prefix =
                    String.slice 0 x word

                rest =
                    String.slice x (String.length word) word
            in
            String.concat [ rest, prefix, "ay" ]

        _ ->
            word


allRules : List Rule
allRules =
    [ ( firstRulePredicate, firstRuleTranslator )
    , ( thirdRulePredicate, thirdRuleTranslator )
    , ( fourthRulePredicate, fourthRuleTranslator )
    , ( secondRulePredicate, secondRuleTranslator )
    ]


translateWord : List Rule -> String -> String
translateWord rules word =
    case rules of
        [] ->
            word

        rule :: otherRules ->
            let
                ( predicate, translator ) =
                    rule
            in
            if predicate word then
                translator word

            else
                translateWord otherRules word


translate : String -> String
translate input =
    input
        |> String.words
        |> List.map (translateWord allRules)
        |> String.join " "
