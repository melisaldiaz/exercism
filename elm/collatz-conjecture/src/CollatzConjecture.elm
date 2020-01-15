module CollatzConjecture exposing (..)


collatz : Int -> Result String Int
collatz start =
    case start <= 0 of
        True ->
            Err "Only positive numbers are allowed"

        False ->
            Ok (List.length (stepsList start))


step : Int -> Int
step n =
    if modBy 2 n == 0 then
        n // 2

    else
        (n * 3) + 1


stepsList : Int -> List Int
stepsList n =
    case n of
        1 ->
            []

        _ ->
            n :: stepsList (step n)
