module ResistorColorTrio exposing (Color(..), label)


type Color
    = Black
    | Brown
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Violet
    | Grey
    | White


colorToValue color =
    case color of
        Black ->
            0

        Brown ->
            1

        Red ->
            2

        Orange ->
            3

        Yellow ->
            4

        Green ->
            5

        Blue ->
            6

        Violet ->
            7

        Grey ->
            8

        White ->
            9


formatValue : Int -> Int -> Int -> String
formatValue firstDigit secondDigit thirdDigit =
    let
        baseValue : Int
        baseValue =
            case secondDigit of
                0 ->
                    firstDigit

                _ ->
                    10 * firstDigit + secondDigit

        totalZero : Int
        totalZero =
            case secondDigit of
                0 ->
                    thirdDigit + 1

                _ ->
                    thirdDigit

        value : Int
        value =
            baseValue * (10 ^ remainderBy 3 totalZero)

        prefix : String
        prefix =
            case totalZero // 3 of
                0 ->
                    ""

                1 ->
                    "kilo"

                2 ->
                    "mega"

                3 ->
                    "giga"

                _ ->
                    ""
    in
    String.concat [ String.fromInt value, " ", prefix, "ohms" ]


label : List Color -> String
label colors =
    case colors of
        firstColor :: secondColor :: thirdColor :: _ ->
            let
                firstDigit =
                    colorToValue firstColor

                secondDigit =
                    colorToValue secondColor

                thirdDigit =
                    colorToValue thirdColor
            in
            formatValue firstDigit secondDigit thirdDigit

        _ ->
            ""
