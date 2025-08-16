module EliudsEggs exposing (eggCount)

import Bitwise

eggCount : Int -> Int
eggCount n =
    let
        egg = Bitwise.and 1 n
        nextN = Bitwise.shiftRightBy 1 n
    in
    if n == 0 then
        0
    else
        egg + eggCount nextN
