module MatchingBrackets exposing (isPaired)

import Dict exposing (Dict)
import Set exposing (Set)


openingBracket : Char
openingBracket =
    '['


closingBracket : Char
closingBracket =
    ']'


openingBrace : Char
openingBrace =
    '{'


closingBrace : Char
closingBrace =
    '}'


openingParenthesis : Char
openingParenthesis =
    '('


closingParenthesis : Char
closingParenthesis =
    ')'


allOpening : Set Char
allOpening =
    Set.fromList [ openingBracket, openingBrace, openingParenthesis ]


allClosing : Set Char
allClosing =
    Set.fromList [ closingBracket, closingBrace, closingParenthesis ]


all : Set Char
all =
    Set.union allOpening allClosing


matchingOpening : Dict Char Char
matchingOpening =
    Dict.fromList
        [ ( closingBracket, openingBracket )
        , ( closingBrace, openingBrace )
        , ( closingParenthesis, openingParenthesis )
        ]


isPairedHelper : List Char -> List Char -> Bool
isPairedHelper stack input =
    case input of
        [] ->
            List.isEmpty stack

        c :: cs ->
            if not <| Set.member c all then
                isPairedHelper stack cs

            else if Set.member c allOpening then
                isPairedHelper (c :: stack) cs

            else
                let
                    opening =
                        Dict.get c matchingOpening

                    stackHead =
                        List.head stack

                    stackTail =
                        List.tail stack
                in
                case ( opening, stackHead, stackTail ) of
                    ( Just o1, Just o2, Just t ) ->
                        if o1 == o2 then
                            isPairedHelper t cs

                        else
                            False

                    _ ->
                        False


isPaired : String -> Bool
isPaired input =
    isPairedHelper [] (String.toList input)
