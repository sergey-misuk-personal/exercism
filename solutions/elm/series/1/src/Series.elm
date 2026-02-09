module Series exposing (slices)


slicesHelper : Int -> List Int -> List (List Int)
slicesHelper size input =
    if size == List.length input then
        [ input ]

    else
        let
            next =
                List.take size input
        in
        next :: (slicesHelper size <| List.drop 1 input)


zeroCode =
    Char.toCode '0'


charToInt : Char -> Int
charToInt char =
    let
        code =
            Char.toCode char
    in
    code - zeroCode


validate : Int -> String -> Maybe String
validate size input =
    if size < 0 then
        Just "slice length cannot be negative"

    else if size == 0 then
        Just "slice length cannot be zero"

    else if String.isEmpty input then
        Just "series cannot be empty"

    else if size > String.length input then
        Just "slice length cannot be greater than series length"

    else
        Nothing


slices : Int -> String -> Result String (List (List Int))
slices size input =
    case validate size input of
        Just error ->
            Err error

        Nothing ->
            let
                digits =
                    List.map charToInt <| String.toList input
            in
            Ok <| slicesHelper size digits
