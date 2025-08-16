module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        trimmed =
            String.trim remark

        isEmpty =
            String.isEmpty trimmed

        isQuestion =
            String.endsWith "?" trimmed

        letters =
            String.filter Char.isAlpha trimmed

        lower =
            String.filter Char.isLower letters

        isAllCapital =
            (String.isEmpty letters |> not) && String.isEmpty lower
    in
    case ( isEmpty, isQuestion, isAllCapital ) of
        ( True, _, _ ) ->
            "Fine. Be that way!"

        ( _, True, True ) ->
            "Calm down, I know what I'm doing!"

        ( _, True, _ ) ->
            "Sure."

        ( _, _, True ) ->
            "Whoa, chill out!"

        _ ->
            "Whatever."
