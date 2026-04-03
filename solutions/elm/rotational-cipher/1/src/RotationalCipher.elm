module RotationalCipher exposing (rotate)


rotateBy : Int -> Char -> Char
rotateBy amount char =
    if Char.isAlpha char then
        let
            baseChar =
                if Char.isUpper char then
                    'A'

                else
                    'a'

            baseCode =
                Char.toCode baseChar

            code =
                Char.toCode char
        in
        Char.fromCode <| baseCode + (modBy 26 <| code - baseCode + amount)

    else
        char


rotate : String -> Int -> String
rotate text shiftKey =
    text
        |> String.toList
        |> List.map (rotateBy shiftKey)
        |> String.fromList
