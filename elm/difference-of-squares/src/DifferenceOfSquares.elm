module DifferenceOfSquares exposing (..)


squareOfSum : Int -> Int
squareOfSum n =
    sqr (sum n)


sumOfSquares : Int -> Int
sumOfSquares n =
    (n * (n + 1) * (2 * n + 1)) // 6


difference : Int -> Int
difference n =
    (squareOfSum n) - (sumOfSquares n)


sum : Int -> Int
sum n =
    (n * (n + 1)) // 2


sqr : Int -> Int
sqr i =
    i * i


