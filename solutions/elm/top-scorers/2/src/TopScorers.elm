module TopScorers exposing (..)

import Dict exposing (Dict)
import TopScorersSupport exposing (PlayerName)


incrementCount maybeCount =
    Just <| Maybe.withDefault 0 maybeCount + 1


updateGoalCountForPlayer : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
updateGoalCountForPlayer playerName playerGoalCounts =
    Dict.update playerName incrementCount playerGoalCounts


aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    List.foldl updateGoalCountForPlayer Dict.empty playerNames


removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    let
        isPlayerSignificant _ counts =
            counts >= goalThreshold
    in
    Dict.filter isPlayerSignificant playerGoalCounts


resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    Dict.insert playerName 0 playerGoalCounts


formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    let
        score =
            Maybe.withDefault 0 <| Dict.get playerName playerGoalCounts
    in
    playerName ++ ": " ++ String.fromInt score


formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    let
        formatPlayerReversedArgs counts name =
            formatPlayer name counts
    in
    players
        |> Dict.keys
        |> List.map (formatPlayerReversedArgs players)
        |> String.join ", "


combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    let
        addCounts name count1 count2 =
            Dict.insert name (count1 + count2)
    in
    Dict.merge Dict.insert addCounts Dict.insert game1 game2 Dict.empty
