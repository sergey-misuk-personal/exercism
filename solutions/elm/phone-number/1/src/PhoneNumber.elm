module PhoneNumber exposing (getNumber)

filterNonDigits : String -> String
filterNonDigits =
    String.filter Char.isDigit

normalizeNumber : String -> Maybe String
normalizeNumber numberString =
    let
        length = String.length numberString
    in
    case length of
        10 -> Just (String.cons '1' numberString)
        11 -> Just numberString
        _ -> Nothing


getCode : Int -> Int -> String -> Int
getCode start end phoneNumber =
    let
        slice = String.slice start end phoneNumber
        number = String.toInt slice
    in
    case number of
        Just n -> n
        Nothing -> 0

checkCode : (String -> Int) -> (Int -> Bool) -> Maybe String -> Maybe String
checkCode codeExtractor codeValidator phoneNumber =
    case phoneNumber of
        Just number ->
            let
                code = codeExtractor number
                isValid = codeValidator code
            in
            if isValid then
                phoneNumber
            else
                Nothing
        Nothing -> Nothing

formatNumber : Maybe String -> Maybe String
formatNumber phoneNumber =
    case phoneNumber of
        Just p -> Just <| String.dropLeft 1 p
        Nothing -> Nothing

getNumber : String -> Maybe String
getNumber numberString =
    numberString
        |> filterNonDigits
        |> normalizeNumber
        |> checkCode (getCode 0 1) ((==) 1)
        |> checkCode (getCode 1 4) ((<=) 200)
        |> checkCode (getCode 4 7) ((<=) 200)
        |> formatNumber
