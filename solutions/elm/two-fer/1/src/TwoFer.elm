module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer name =
    let
        actualName =
            case name of
                Just n ->
                    n

                Nothing ->
                    "you"
    in
    "One for " ++ actualName ++ ", one for me."
