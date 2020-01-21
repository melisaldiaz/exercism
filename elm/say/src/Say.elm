module Say exposing (SayError(..), say)

import Dict exposing (Dict)


type SayError
    = Negative
    | TooLarge


say : Int -> Result SayError String
say number =
    let
        digits : List Int
        digits =
            toDigits number

        groups : List (List Int)
        groups =
            List.reverse (List.map List.reverse (splitEvery 3 (List.reverse digits)))
    in
    case sayError number of
        Just e ->
            Err e

        Nothing ->
            if number == 0 then
                Ok "zero"

            else
                Ok (scaling (List.map groupsOfThree groups))


sayError : Int -> Maybe SayError
sayError number =
    if number < 0 then
        Just Negative

    else if number > 999999999999 then
        Just TooLarge

    else
        Nothing



-- Receives a list of one to three elements.


groupsOfThree : List Int -> ( String, Int )
groupsOfThree list =
    let
        n : Int
        n =
            Maybe.withDefault 0 (intFromList list)
    in
    case list of
        [ trd ] ->
            ( units [ trd ], n )

        snd :: trd :: [] ->
            ( tens [ snd, trd ], n )

        fst :: snd :: trd :: [] ->
            ( hundreds [ fst, snd, trd ], n )

        _ ->
            ( "", n )


hundreds : List Int -> String
hundreds list =
    let
        n : Int
        n =
            Maybe.withDefault 0 (intFromList list)
    in
    case list of
        x :: y :: z :: [] ->
            if x == 0 && y == 0 && z == 0 then
                ""

            else if x == 0 && y == 0 then
                units [ z ]

            else if x == 0 then
                "" ++ tens [ y, z ]

            else
                let
                    amount : String
                    amount =
                        Maybe.withDefault "" (Dict.get (n // 100) inEnglish)
                in
                if remainderBy 100 n == 0 then
                    amount ++ " hundred"

                else
                    amount ++ " hundred and " ++ tens [ y, z ]

        _ ->
            ""


tens : List Int -> String
tens list =
    case list of
        x :: y :: [] ->
            let
                ty : Int
                ty =
                    Maybe.withDefault 0 (intFromList (x :: [ 0 ]))

                tyWord : String
                tyWord =
                    Maybe.withDefault "" (Dict.get ty inEnglish)

                teen : Int
                teen =
                    Maybe.withDefault 0 (intFromList [ x, y ])
            in
            if x >= 1 && y == 0 then
                tyWord

            else if x >= 2 && y >= 1 then
                tyWord ++ "-" ++ units [ y ]

            else if x >= 1 && y >= 1 then
                Maybe.withDefault "" (Dict.get teen inEnglish)

            else if x == 0 && y >= 1 then
                units [ y ]

            else
                ""

        _ ->
            ""


units : List Int -> String
units list =
    case list of
        [ x ] ->
            Maybe.withDefault "" (Dict.get x inEnglish)

        _ ->
            ""



-- Given a tuple, whose first element is its index in a bigger list, and whose
-- second element is a tuple of a written number and its numeric value, it
-- returns a reversed list of the written number plus its large number name
--according to its index.
-- Example: (4, ("one hundred and ten",110)) -> ["billion","one hundred and ten"]


largeNumbers : ( Int, ( String, Int ) ) -> List String
largeNumbers x =
    let
        ln : ( Int, ( String, Int ) ) -> Maybe (List String)
        ln ( index, ( words, value ) ) =
            let
                w : Bool
                w =
                    String.isEmpty words
            in
            case index of
                0 ->
                    if value >= 1 && value <= 99 then
                        Just [ words, "and" ]

                    else if w then
                        Just []

                    else
                        Just [ words ]

                1 ->
                    if w then
                        Just []

                    else
                        Just [ "thousand", words ]

                2 ->
                    if w then
                        Just []

                    else
                        Just [ "million", words ]

                3 ->
                    if w then
                        Just []

                    else
                        Just [ "billion", words ]

                _ ->
                    Nothing
    in
    Maybe.withDefault [] (ln x)


scaling : List ( String, Int ) -> String
scaling list =
    let
        l : List ( Int, ( String, Int ) )
        l =
            List.indexedMap Tuple.pair (List.reverse list)
    in
    if List.length list <= 1 then
        case list of
            [] ->
                ""

            a :: _ ->
                Tuple.first a

    else
        String.concat (List.intersperse " " (List.reverse (List.concat (List.map largeNumbers l))))


inEnglish : Dict Int String
inEnglish =
    Dict.fromList
        [ ( 1, "one" )
        , ( 2, "two" )
        , ( 3, "three" )
        , ( 4, "four" )
        , ( 5, "five" )
        , ( 6, "six" )
        , ( 7, "seven" )
        , ( 8, "eight" )
        , ( 9, "nine" )
        , ( 10, "ten" )
        , ( 11, "eleven" )
        , ( 12, "twelve" )
        , ( 13, "thirteen" )
        , ( 14, "fourteen" )
        , ( 15, "fifteen" )
        , ( 16, "sixteen" )
        , ( 17, "seventeen" )
        , ( 18, "eighteen" )
        , ( 19, "nineteen" )
        , ( 20, "twenty" )
        , ( 30, "thirty" )
        , ( 40, "forty" )
        , ( 50, "fifty" )
        , ( 60, "sixty" )
        , ( 70, "seventy" )
        , ( 80, "eighty" )
        , ( 90, "ninety" )
        ]


toDigits : Int -> List Int
toDigits n =
    case n < 10 of
        True ->
            [ n ]

        False ->
            toDigits (floor (toFloat n / 10)) ++ [ modBy 10 n ]


splitEvery : Int -> List Int -> List (List Int)
splitEvery n numbers =
    case numbers of
        [] ->
            []

        _ ->
            case n >= List.length numbers of
                True ->
                    [ numbers ]

                False ->
                    List.take n numbers :: splitEvery n (List.drop n numbers)


intFromList : List Int -> Maybe Int
intFromList list =
    let
        length =
            List.length list
    in
    case list of
        [] ->
            Nothing

        a :: rest ->
            Just (a * (10 ^ (length - 1)) + Maybe.withDefault 0 (intFromList rest))
