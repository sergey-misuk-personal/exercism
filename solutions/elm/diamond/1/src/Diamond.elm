module Diamond exposing (rows)


rows : Char -> String
rows letter =
    let
        charToIndex : Char -> Int
        charToIndex c =
            Char.toCode c - Char.toCode 'A'

        indexToChar : Int -> Char
        indexToChar i =
            Char.fromCode <| Char.toCode 'A' + i

        maxIndex =
            charToIndex letter

        lineWidth : Int
        lineWidth =
            maxIndex * 2 + 1

        topIndices =
            List.range 0 maxIndex

        indexToLine w i =
            let
                l =
                    String.fromChar <| indexToChar i

                indent =
                    String.repeat (w // 2 - i) "_"

                spacing =
                    if i == 0 then
                        ""

                    else
                        String.repeat (2 * i - 1) "_"

                secondL =
                    if i == 0 then
                        ""

                    else
                        l
            in
            String.concat [ indent, l, spacing, secondL, indent ]

        topStrings =
            List.map (indexToLine lineWidth) topIndices

        bottomString =
            case List.tail <| List.reverse topStrings of
                Just strings ->
                    strings

                Nothing ->
                    []
    in
    String.join "\n" (topStrings ++ bottomString)
