module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight ({ bearing } as robot) =
    let
        newBearing : Bearing
        newBearing =
            case bearing of
                North ->
                    East

                East ->
                    South

                South ->
                    West

                West ->
                    North
    in
    { robot | bearing = newBearing }


turnLeft : Robot -> Robot
turnLeft ({ bearing } as robot) =
    let
        newBearing : Bearing
        newBearing =
            case bearing of
                North ->
                    West

                West ->
                    South

                South ->
                    East

                East ->
                    North
    in
    { robot | bearing = newBearing }


advance : Robot -> Robot
advance ({ bearing, coordinates } as robot) =
    let
        { x, y } =
            coordinates

        newCoordinates : { x : Int, y : Int }
        newCoordinates =
            case bearing of
                North ->
                    { coordinates | y = y + 1 }

                East ->
                    { coordinates | x = x + 1 }

                South ->
                    { coordinates | y = y - 1 }

                West ->
                    { coordinates | x = x - 1 }
    in
    { robot | coordinates = newCoordinates }


handleCommand : Char -> Robot -> Robot
handleCommand command robot =
    case command of
        'L' ->
            turnLeft robot

        'R' ->
            turnRight robot

        'A' ->
            advance robot

        _ ->
            robot


simulate : String -> Robot -> Robot
simulate directions robot =
    directions
        |> String.toList
        |> List.foldl handleCommand robot
