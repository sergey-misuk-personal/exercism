module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let
        isNotSpace : Char -> Bool
        isNotSpace ch =
            ch /= ' '

        chars =
            input
                |> String.toList
                |> List.filter isNotSpace

        tooShort : List Char -> Bool
        tooShort chs =
            List.length chs < 2

        anyNonDigit : List Char -> Bool
        anyNonDigit =
            List.any <| Char.isDigit >> not
    in
    if tooShort chars || anyNonDigit chars then
        False

    else
        let
            charToDigits : Char -> Int
            charToDigits ch =
                Char.toCode ch - Char.toCode '0'

            doubleEverySecond : Int -> Int -> Int
            doubleEverySecond i d =
                if modBy 2 i == 1 then
                    2 * d

                else
                    d

            applyLimit : Int -> Int
            applyLimit d =
                if d > 9 then
                    d - 9

                else
                    d
        in
        chars
            |> List.reverse
            |> List.map charToDigits
            |> List.indexedMap doubleEverySecond
            |> List.map applyLimit
            |> List.sum
            |> modBy 10
            |> (==) 0
