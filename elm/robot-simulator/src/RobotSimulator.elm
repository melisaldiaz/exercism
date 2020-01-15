module RobotSimulator exposing (..)


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
turnRight robot =
    case robot.bearing of
        North ->
            { robot | bearing = East }

        East ->
            { robot | bearing = South }

        South ->
            { robot | bearing = West }

        West ->
            { robot | bearing = North }


turnLeft : Robot -> Robot
turnLeft robot =
    case robot.bearing of
        North ->
            { robot | bearing = West }

        West ->
            { robot | bearing = South }

        South ->
            { robot | bearing = East }

        East ->
            { robot | bearing = North }


advance : Robot -> Robot
advance robot =
    let
        c =
            robot.coordinates
    in
    { robot
        | coordinates =
            case robot.bearing of
                North ->
                    { c | y = robot.coordinates.y + 1 }

                South ->
                    { c | y = robot.coordinates.y - 1 }

                East ->
                    { c | x = robot.coordinates.x + 1 }

                West ->
                    { c | x = robot.coordinates.x - 1 }
    }


simulate : String -> Robot -> Robot
simulate directions robot =
    case directions of
        "" ->
            robot

        _ ->
            case String.uncons directions of
                Nothing ->
                    robot

                Just ( a, rest ) ->
                    case a of
                        'R' ->
                            simulate rest (turnRight robot)

                        'L' ->
                            simulate rest (turnLeft robot)

                        'A' ->
                            simulate rest (advance robot)

                        _ ->
                            simulate rest robot
