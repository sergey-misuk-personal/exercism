module SpiralMatrix exposing (spiralMatrix)


type Direction
    = Up
    | Right
    | Down
    | Left


numbers : Int -> List Int
numbers size =
    List.range 1 <| size ^ 2


indicesHelper : Int -> Direction -> Int -> Int -> Int -> List Int
indicesHelper size direction total current index =
    if total == 0 then
        []

    else if current == total then
        let
            nextDirection : Direction
            nextDirection =
                case direction of
                    Right ->
                        Down

                    Down ->
                        Left

                    Left ->
                        Up

                    Up ->
                        Right

            nextTotal : Int
            nextTotal =
                if nextDirection == Down || nextDirection == Up then
                    total - 1

                else
                    total
        in
        indicesHelper size nextDirection nextTotal 0 index

    else
        let
            nextCurrent : Int
            nextCurrent =
                current + 1

            nextIndex =
                case direction of
                    Right ->
                        index + 1

                    Down ->
                        index + size

                    Left ->
                        index - 1

                    Up ->
                        index - size
        in
        nextIndex :: indicesHelper size direction total nextCurrent nextIndex


indices : Int -> List Int
indices size =
    indicesHelper size Right size 0 -1


batch : Int -> List a -> List (List a)
batch size data =
    if List.isEmpty data then
        []

    else
        List.take size data :: batch size (List.drop size data)


spiralMatrix : Int -> List (List Int)
spiralMatrix size =
    List.map2 Tuple.pair (indices size) (numbers size)
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> batch size
