module PythagoreanTriplet exposing (triplets)


type alias Triplet =
    ( Int, Int, Int )


triplets : Int -> List Triplet
triplets n =
    let
        aValues : List Int
        aValues =
            List.range 1 (n // 3 - 1)

        isPythagorean : Triplet -> Bool
        isPythagorean ( a, b, c ) =
            a ^ 2 + b ^ 2 == c ^ 2

        makeTripletFromAAndB : Int -> Int -> Triplet
        makeTripletFromAAndB a b =
            ( a, b, n - a - b )

        makePythagoreanTripletsFromA : Int -> List Triplet
        makePythagoreanTripletsFromA a =
            let
                minB : Int
                minB =
                    a + 1

                maxB : Int
                maxB =
                    ceiling (toFloat (n - a) / 2) - 1
            in
            List.range minB maxB
                |> List.map (makeTripletFromAAndB a)
                |> List.filter isPythagorean
    in
    aValues
        |> List.concatMap makePythagoreanTripletsFromA
