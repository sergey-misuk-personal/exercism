module ScrabbleScore exposing (scoreWord)

import Dict exposing (Dict)

groupedLetterValues : Dict Int (List Char)
groupedLetterValues =
    Dict.fromList [
        (1,  ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T']),
        (2,  ['D', 'G']),
        (3,  ['B', 'C', 'M', 'P']),
        (4,  ['F', 'H', 'V', 'W', 'Y']),
        (5,  ['K']),
        (8,  ['J', 'X']),
        (10, ['Q', 'Z'])
    ]


letterValues =
    groupedLetterValues
        |> Dict.toList
        |> List.concatMap (\(value, letters) -> List.map (\l -> (l, value)) letters)
        |> Dict.fromList


getOrDefault dict default key =
    let
        value = Dict.get key dict
    in
    case value of
        Just v -> v
        Nothing -> default


scoreWord : String -> Int
scoreWord x =
    x
        |> String.toUpper
        |> String.toList
        |> List.map (getOrDefault letterValues 0)
        |> List.sum
