module SumOfMultiples exposing (..)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    let
        ms =
            noDuplicates (flatten (List.map (multiples limit) divisors))
    in
    List.foldl (+) 0 ms


multiples : Int -> Int -> List Int
multiples limit divisor =
    case limit of
        0 ->
            []

        n ->
            if modBy divisor (n - 1) == 0 then
                (n - 1) :: multiples (n - 1) divisor

            else
                multiples (n - 1) divisor


flatten : List (List a) -> List a
flatten =
    \lla ->
        List.foldr (++) [] lla


noDuplicates : List a -> List a
noDuplicates xs =
    case xs of
        [] ->
            []

        a :: rest ->
            a :: noDuplicates (List.filter (\b -> a /= b) rest)
