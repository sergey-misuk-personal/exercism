module Go exposing (..)

import GoSupport exposing (..)


applyRule : Rule -> Result String Game -> Result String Game
applyRule rule result =
    case result of
        Ok modifiedGame ->
            rule modifiedGame

        Err _ ->
            result


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        White ->
            Black

        Black ->
            White


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    let
        initial =
            Ok game

        rules =
            [ oneStonePerPointRule, Ok << captureRule, libertyRule, koRule ]
    in
    case List.foldl applyRule initial rules of
        Ok modifiedGame ->
            { modifiedGame | player = switchPlayer modifiedGame.player }

        Err error ->
            { game | error = error }
