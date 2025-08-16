module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)


square : Int -> Int
square n = n ^ 2

squareOfSum : Int -> Int
squareOfSum n =
    List.range 1 n
        |> List.sum
        |> square
    


sumOfSquares : Int -> Int
sumOfSquares n =
    List.range 1 n
        |> List.map square
        |> List.sum


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n
