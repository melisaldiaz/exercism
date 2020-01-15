module Triangle exposing (..)


rows : Int -> List (List Int)
rows n =
    if n < 1 then
        []

    else
        rows (n - 1) ++ [ List.map (cell (n - 1)) (List.range 0 (n - 1)) ]


cell : Int -> Int -> Int
cell row col =
    factorial row // (factorial col * factorial (row - col))


factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)
