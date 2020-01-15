module Grains exposing (..)


square : Int -> Maybe Int
square n =
    case n > 0 of
        True -> Just (2 ^ (n - 1))
        False -> Nothing
