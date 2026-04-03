module KindergartenGarden exposing (Plant(..), Student(..), plants)


type Student
    = Alice
    | Bob
    | Charlie
    | David
    | Eve
    | Fred
    | Ginny
    | Harriet
    | Ileana
    | Joseph
    | Kincaid
    | Larry


studentToIndex : Student -> Int
studentToIndex student =
    case student of
        Alice ->
            0

        Bob ->
            1

        Charlie ->
            2

        David ->
            3

        Eve ->
            4

        Fred ->
            5

        Ginny ->
            6

        Harriet ->
            7

        Ileana ->
            8

        Joseph ->
            9

        Kincaid ->
            10

        Larry ->
            11


type Plant
    = Grass
    | Clover
    | Radish
    | Violet


letterToPlant : Char -> Plant
letterToPlant letter =
    case letter of
        'G' ->
            Grass

        'C' ->
            Clover

        'R' ->
            Radish

        _ ->
            Violet


plants : String -> Student -> List Plant
plants diagram student =
    let
        start : Int
        start =
            2 * studentToIndex student

        end : Int
        end =
            start + 2

        rows : List String
        rows =
            String.lines diagram
    in
    rows
        |> List.map (String.slice start end)
        |> List.concatMap String.toList
        |> List.map letterToPlant
