module Pangram exposing (..)

import Set exposing (Set)


isPangram : String -> Bool
isPangram sentence =
    containsAll alphabet (String.toLower sentence)


containsAll : Set Char -> String -> Bool
containsAll pending s =
    case Set.isEmpty pending of
        True ->
            True

        False ->
            case String.uncons s of
                Nothing ->
                    False

                Just ( a, rest ) ->
                    containsAll (Set.remove a pending) rest



alphabet : Set Char
alphabet =
    Set.fromList
        [ 'a'
        , 'b'
        , 'c'
        , 'd'
        , 'e'
        , 'f'
        , 'g'
        , 'h'
        , 'i'
        , 'j'
        , 'k'
        , 'l'
        , 'm'
        , 'n'
        , 'o'
        , 'p'
        , 'q'
        , 'r'
        , 's'
        , 't'
        , 'u'
        , 'v'
        , 'w'
        , 'x'
        , 'y'
        , 'z'
        ]


isPangram2 : String -> Bool
isPangram2 sentence =
    let s = 
            sentence 
                |> String.toLower
                |> String.toList
                |> Set.fromList
    
    in 
        Set.isEmpty (Set.diff alphabet s)
