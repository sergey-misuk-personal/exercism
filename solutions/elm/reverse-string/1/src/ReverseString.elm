module ReverseString exposing (reverse)


reverse : String -> String
reverse =
    String.toList >> List.reverse >> String.fromList
