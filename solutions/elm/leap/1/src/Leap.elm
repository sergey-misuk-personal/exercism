module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    let
        divisibleBy4 = modBy 4 year == 0
        divisibleBy100 = modBy 100 year == 0
        divisibleBy400 = modBy 400 year == 0
    in
        divisibleBy4 && (not divisibleBy100) || divisibleBy4 && divisibleBy400
