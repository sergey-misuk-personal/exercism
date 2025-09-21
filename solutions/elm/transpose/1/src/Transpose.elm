module Transpose exposing (transpose)


transposeHelper : List (List Char) -> List (List Char)
transposeHelper matrix =
    let
        headColumn : List (Maybe Char)
        headColumn =
            List.map List.head matrix
    in
    if List.all ((==) Nothing) headColumn then
        []

    else
        let
            tailColumns : List (List Char)
            tailColumns =
                List.map (Maybe.withDefault [] << List.tail) matrix

            handleSpacesHelper : Bool -> List (Maybe Char) -> List Char
            handleSpacesHelper seenNonSpace maybeChars =
                case maybeChars of
                    maybeChar :: rest ->
                        case maybeChar of
                            Just char ->
                                char :: handleSpacesHelper True rest

                            Nothing ->
                                if seenNonSpace then
                                    ' ' :: handleSpacesHelper seenNonSpace rest

                                else
                                    handleSpacesHelper seenNonSpace rest

                    [] ->
                        []

            handleSpaces : List (Maybe Char) -> List Char
            handleSpaces =
                List.reverse << handleSpacesHelper False << List.reverse
        in
        handleSpaces headColumn :: transposeHelper tailColumns


transpose : List String -> List String
transpose lines =
    lines
        |> List.map String.toList
        |> transposeHelper
        |> List.map String.fromList
