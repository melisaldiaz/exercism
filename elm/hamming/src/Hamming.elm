module Hamming exposing (..)


distance : String -> String -> Result String Int
distance left right =
    case strandDifferences (String.toList left) (String.toList right) of
        Nothing ->
            Err "left and right strands must be of equal length"

        Just i ->
            Ok i


strandDifferences : List a -> List a -> Maybe Int
strandDifferences la lb =
    case ( la, lb ) of
        ( [], [] ) ->
            Just 0

        ( a :: rest1, b :: rest2 ) ->
            case a /= b of
                True ->
                    Maybe.map (\x -> x + 1)
                        (strandDifferences rest1 rest2)

                False ->
                    strandDifferences rest1 rest2

        _ ->
            Nothing
