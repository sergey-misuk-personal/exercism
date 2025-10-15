module AtbashCipher exposing (decode, encode)


convertLowercaseAlphaNum : Char -> Char
convertLowercaseAlphaNum c =
    if Char.isDigit c then
        c

    else
        let
            offset : Int
            offset =
                Char.toCode c - Char.toCode 'a'

            invertedCode =
                Char.toCode 'z' - offset
        in
        Char.fromCode invertedCode


encode : String -> String
encode plain =
    let
        groupBy5 : List Char -> List (List Char)
        groupBy5 cs =
            case cs of
                [] ->
                    []

                _ ->
                    let
                        csHead : List Char
                        csHead =
                            List.take 5 cs

                        csTail : List Char
                        csTail =
                            List.drop 5 cs
                    in
                    csHead :: groupBy5 csTail
    in
    plain
        |> String.toLower
        |> String.toList
        |> List.filter Char.isAlphaNum
        |> List.map convertLowercaseAlphaNum
        |> groupBy5
        |> List.map String.fromList
        |> String.join " "


decode : String -> String
decode cipher =
    cipher
        |> String.toList
        |> List.filter Char.isAlphaNum
        |> List.map convertLowercaseAlphaNum
        |> String.fromList
