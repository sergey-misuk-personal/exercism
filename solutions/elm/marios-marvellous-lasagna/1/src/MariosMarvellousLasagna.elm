module MariosMarvellousLasagna exposing (remainingTimeInMinutes)


remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes layers minutesSinceStart =
    let
        expectedMinutesInOven =
            40

        preparationTimeInMinutes l =
            2 * l
    in
    preparationTimeInMinutes layers + expectedMinutesInOven - minutesSinceStart
