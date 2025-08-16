module Pangram exposing (isPangram)

import Set


isPangram : String -> Bool
isPangram sentence =
    sentence
        |> String.toLower
        |> String.toList
        |> List.filter Char.isAlpha
        |> Set.fromList
        |> Set.size
        |> (==) 26
