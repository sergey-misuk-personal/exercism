module RunLengthEncoding exposing (decode, encode)


encodeHelper : Maybe Char -> Int -> List Char -> String
encodeHelper previousChar counter chars =
    let
        encodeChar : Char -> Int -> String
        encodeChar ch c =
            if c == 1 then
                String.fromChar ch

            else
                String.fromInt c ++ String.fromChar ch
    in
    case chars of
        firstChar :: rest ->
            case previousChar of
                Just char ->
                    if firstChar == char then
                        encodeHelper previousChar (counter + 1) rest

                    else
                        encodeChar char counter ++ encodeHelper (Just firstChar) 1 rest

                Nothing ->
                    encodeHelper (Just firstChar) 1 rest

        [] ->
            case previousChar of
                Just char ->
                    encodeChar char counter

                Nothing ->
                    ""


encode : String -> String
encode string =
    encodeHelper Nothing 0 (String.toList string)


decodeHelper : Int -> List Char -> String
decodeHelper counter chars =
    let
        digitToNumber : Char -> Int
        digitToNumber ch =
            Char.toCode ch - Char.toCode '0'
    in
    case chars of
        firstChar :: rest ->
            if Char.isDigit firstChar then
                decodeHelper (10 * counter + digitToNumber firstChar) rest

            else if counter > 0 then
                String.repeat counter (String.fromChar firstChar) ++ decodeHelper 0 rest

            else
                String.fromChar firstChar ++ decodeHelper 0 rest

        [] ->
            ""


decode : String -> String
decode string =
    decodeHelper 0 (String.toList string)
