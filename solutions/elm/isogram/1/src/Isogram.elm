module Isogram exposing (isIsogram)

import Set


isIsogram : String -> Bool
isIsogram sentence =
    let
        letterList =
            sentence
                |> String.toLower
                |> String.toList
                |> List.filter Char.isAlpha

        letterSet =
            Set.fromList letterList
    in
    List.length letterList == Set.size letterSet
