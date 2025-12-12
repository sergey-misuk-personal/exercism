module RomanNumerals exposing (toRoman)


type alias RomanDigit =
    ( Int, String )


type alias ConvertedNumber =
    ( Int, String )


romanDigits : List RomanDigit
romanDigits =
    [ ( 1000, "M" )
    , ( 900, "CM" )
    , ( 500, "D" )
    , ( 400, "CD" )
    , ( 100, "C" )
    , ( 90, "XC" )
    , ( 50, "L" )
    , ( 40, "XL" )
    , ( 10, "X" )
    , ( 9, "IX" )
    , ( 5, "V" )
    , ( 4, "IV" )
    , ( 1, "I" )
    ]


applyDigit : RomanDigit -> ConvertedNumber -> ConvertedNumber
applyDigit ( romanDigitValue, romanDigitName ) ( decimalValue, romanValue ) =
    let
        digitNumber =
            decimalValue // romanDigitValue
    in
    if digitNumber > 0 then
        ( remainderBy romanDigitValue decimalValue
        , String.append romanValue <| String.repeat digitNumber romanDigitName
        )

    else
        ( decimalValue, romanValue )


toRoman : Int -> String
toRoman number =
    Tuple.second <| List.foldl applyDigit ( number, "" ) romanDigits
