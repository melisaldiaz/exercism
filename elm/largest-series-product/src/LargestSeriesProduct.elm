module LargestSeriesProduct exposing (..)


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    if length < 0 then
        Nothing

    else if length == 0 then
        Just 1

    else
        case String.all Char.isDigit series of
            False ->
                Nothing

            True ->
                case length > String.length series of
                    True ->
                        Nothing

                    False ->
                        let
                            digits : List Int
                            digits =
                                stringToDigits series

                            groups : List (List Int)
                            groups =
                                group length digits []

                            products : List Int
                            products =
                                List.concat [ List.map List.product groups ]
                        in
                        List.maximum products






-- Given a number n and a list of numbers, it returns a list of lists of n adjacent numbers.
-- Example 3 [1,2,3,4,5,6,7] -> [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[]]
group : Int -> List Int -> List (List Int) -> List (List Int)
group n list acc =
    case n > List.length list of
        True ->
            acc

        False ->
            List.take n list :: group n (List.drop 1 list) acc


stringToDigits : String -> List Int
stringToDigits s =
    case s of
        "" ->
            []

        _ ->
            let
                chars : List Char
                chars =
                    String.toList s
            in
            case sequence (List.map charToInt chars) of
                Nothing ->
                    []

                Just x ->
                    x

sequence : List (Maybe x) -> Maybe (List x)
sequence lm =
    case lm of
        [] ->
            Just []

        a :: rest ->
            Maybe.map2 (::) a (sequence rest)


charToInt : Char -> Maybe Int
charToInt c =
    case c of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        _ ->
            Nothing


