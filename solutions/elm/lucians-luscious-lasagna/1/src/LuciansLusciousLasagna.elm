module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

expectedMinutesInOven = 40
preparationTimeInMinutes n = 2 * n
elapsedTimeInMinutes n s = preparationTimeInMinutes n + s
