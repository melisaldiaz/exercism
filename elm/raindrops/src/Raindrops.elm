module Raindrops exposing (..)


raindrops : Int -> String
raindrops number =
    let
        x =
            if modBy 3 number == 0 then
                "Pling"

            else
                ""

        y =
            if modBy 5 number == 0 then
                "Plang"

            else
                ""

        z =
            if modBy 7 number == 0 then
                "Plong"

            else
                ""
        xyz = x ++ y ++ z
    in
    case String.isEmpty xyz of
        True ->
            String.fromInt number

        False ->
            xyz

