module BirdCount exposing (busyDays, hasDayWithoutBirds, incrementDayCount, today, total)


today : List Int -> Maybe Int
today counts =
    case counts of
        x :: _ ->
            Just x

        [] ->
            Nothing


incrementDayCount : List Int -> List Int
incrementDayCount counts =
    case counts of
        x :: xs ->
            (x + 1) :: xs

        [] ->
            [ 1 ]


hasDayWithoutBirds : List Int -> Bool
hasDayWithoutBirds counts =
    case counts of
        x :: xs ->
            if x == 0 then
                True

            else
                hasDayWithoutBirds xs

        [] ->
            False


total : List Int -> Int
total counts =
    case counts of
        x :: xs ->
            x + total xs

        [] ->
            0


busyDays : List Int -> Int
busyDays counts =
    case counts of
        x :: xs ->
            let
                inc =
                    if x >= 5 then
                        1

                    else
                        0
            in
            inc + busyDays xs

        [] ->
            0
