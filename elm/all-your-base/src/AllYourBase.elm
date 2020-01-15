module AllYourBase exposing (..)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    let
        f : Int -> List Int -> List Int
        f n acc =
            case n of
                0 ->
                    acc

                _ ->
                    f (n // outBase) ( remainderBy outBase n :: acc)
    in
    case
        List.all identity
            [ inBase > 0
            , outBase > 0
            , not (List.all (\x -> x == 0) digits)
            , List.all (\x -> x >= 0 && x < inBase) digits
            ]
    of
        False ->
            Nothing

        True ->
            Just (f (toDecimal inBase digits) [])



toDecimal : Int -> List Int -> Int
toDecimal inBase digits =
    List.sum
        (List.map
            (\a -> Tuple.second a * inBase ^ Tuple.first a)
            (indexedList digits)
        )



-- returns a list of the digits in reverse order with their corresponding index.
-- example: [3,5] -> [(0,5), (1,3)]
indexedList : List Int -> List ( Int, Int )
indexedList digits =
    List.indexedMap Tuple.pair (List.reverse digits)
