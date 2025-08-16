module Etl exposing (transform)

import Dict exposing (Dict)

unpack : (Int, List String) -> List (String, Int)
unpack (points, letters) =
    let
        lowerCaseLetters = List.map String.toLower letters
    in
    List.map (\letter -> (String.toLower letter, points)) lowerCaseLetters

transform : Dict Int (List String) -> Dict String Int
transform input =
    input
        |> Dict.toList
        |> List.concatMap unpack
        |> Dict.fromList
