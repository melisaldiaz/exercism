module BinarySearch exposing (..)

import Array exposing (Array)


find : Int -> Array Int -> Maybe Int
find target xs =
    let
        index =
            Array.length xs // 2
    in
    case Array.get index xs of
        Nothing ->
            Nothing

        Just x ->
            if x > target then
                find target (Array.slice 0 index xs)

            else if x < target then
                Maybe.map (\y -> y + index + 1)
                    (find target (Array.slice (index + 1) (Array.length xs) xs))

            else
                Just index
