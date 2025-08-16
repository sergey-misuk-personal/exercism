module SumOfMultiples exposing (sumOfMultiples)

import Set exposing (Set)


multiplesLessThan : Int -> Int -> Set Int
multiplesLessThan limit n =
    List.range 1 ((limit - 1) // n)
        |> List.map ((*) n)
        |> Set.fromList


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    divisors
        |> List.map (multiplesLessThan limit)
        |> List.foldl Set.union Set.empty
        |> Set.toList
        |> List.sum
