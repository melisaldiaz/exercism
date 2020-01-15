module ScrabbleScore exposing (..)

import Dict exposing (Dict)


scoreWord : String -> Int
scoreWord x =
    let
        y : List Char
        y =
            List.map Char.toUpper (String.toList x)

        dict : Dict Char Int
        dict =
            makeDict scores

        g : Char -> Int
        g c =
            Maybe.withDefault 0 (Dict.get c dict)
    in
    List.sum (List.map g y)


scores : List ( List Char, Int )
scores =
    [ ( [ 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T' ], 1 )
    , ( [ 'D', 'G' ], 2 )
    , ( [ 'B', 'C', 'M', 'P' ], 3 )
    , ( [ 'F', 'H', 'V', 'W', 'Y' ], 4 )
    , ( [ 'K' ], 5 )
    , ( [ 'J', 'X' ], 8 )
    , ( [ 'Q', 'Z' ], 10 )
    ]


makeDict : List ( List Char, Int ) -> Dict Char Int
makeDict values =
    let
        g : ( List Char, Int ) -> List ( Char, Int )
        g x =
            List.map (\l -> ( l, Tuple.second x )) (Tuple.first x)
    in
    Dict.fromList (List.concat (List.map g values))
