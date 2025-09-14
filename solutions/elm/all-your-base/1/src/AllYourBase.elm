module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    let
        isNotEmpty : List Int -> Bool
        isNotEmpty =
            not << List.isEmpty

        nonNegative : Int -> Bool
        nonNegative =
            (<=) 0

        isZero =
            (==) 0

        fitIntoBase : Int -> Bool
        fitIntoBase =
            (>) inBase

        digitsChecks =
            [ isNotEmpty
            , List.all nonNegative
            , List.all fitIntoBase
            , not << List.all isZero
            ]

        runCheck check =
            check digits

        validBase =
            (<=) 2
    in
    if (not <| List.all runCheck digitsChecks) || (not <| List.all validBase [ inBase, outBase ]) then
        Nothing

    else
        let
            toNumber p d =
                d * (inBase ^ p)

            number =
                List.sum <| List.indexedMap toNumber <| List.reverse digits

            toReversedDigits n =
                if n > 0 then
                    let
                        ( nextNumber, reminder ) =
                            ( n // outBase, modBy outBase n )
                    in
                    reminder :: toReversedDigits nextNumber

                else
                    []
        in
        Just <| List.reverse <| toReversedDigits number
