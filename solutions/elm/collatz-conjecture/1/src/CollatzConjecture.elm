module CollatzConjecture exposing (collatz)

collatzHelper : Int -> Int -> Int
collatzHelper n step =
    if n == 1 then
        step
    else
        if modBy 2 n == 0 then
            collatzHelper (n // 2) (step + 1)
        else
            collatzHelper (3 * n + 1) (step + 1)

collatz : Int -> Result String Int
collatz start =
    if start < 1 then
        Err "Only positive integers are allowed"
    else
        Ok <| collatzHelper start 0
