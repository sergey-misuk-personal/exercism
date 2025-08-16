module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)


clean1 : String -> String
clean1 str =
    str
        |> String.replace " " "_"


clean2 : String -> String
clean2 str =
    let
        controlCharacters : List String
        controlCharacters =
            [ "\n", "\t", "\u{000D}" ]

        ctrl =
            "[CTRL]"
    in
    List.foldl (\cc -> String.replace cc ctrl) str controlCharacters
        |> clean1


clean3 : String -> String
clean3 str =
    let
        kebabToCamel : List Char -> List Char
        kebabToCamel cs =
            case cs of
                [] ->
                    []

                '-' :: [] ->
                    []

                '-' :: c :: ccs ->
                    Char.toUpper c :: kebabToCamel ccs

                c :: ccs ->
                    c :: kebabToCamel ccs
    in
    str
        |> String.toList
        |> kebabToCamel
        |> String.fromList
        |> clean2


clean4 : String -> String
clean4 str =
    str
        |> clean3
        |> String.filter (\c -> not <| Char.isDigit c)


clean : String -> String
clean str =
    str
        |> clean4
        |> String.filter (\c -> c < 'α' || c > 'ω')
