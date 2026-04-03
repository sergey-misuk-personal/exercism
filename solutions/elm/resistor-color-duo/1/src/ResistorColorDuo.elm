module ResistorColorDuo exposing (Color(..), value)


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


colorValue : Color -> Int
colorValue color =
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


value : List Color -> Int
value colors =
    case colors of
        first :: second :: _ ->
            10 * colorValue first + colorValue second

        _ ->
            0
