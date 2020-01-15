module Anagram exposing (..)


detect : String -> List String -> List String
detect word candidates =
    List.filter (isAnagram word) candidates



isAnagram : String -> String -> Bool
isAnagram first second =
    let
        f = String.toLower first

        s = String.toLower second
    in
    case f == s of
        True ->
            False

        False ->
            anagramHelper
                (String.toList f)
                False
                (String.toList s)


anagramHelper : List Char -> Bool -> List Char -> Bool
anagramHelper orig acc poss =
    case ( orig, poss ) of
        ( [], [] ) ->
            acc

        ( _ :: _, p :: ps ) ->
            case List.member p orig of
                False ->
                    False

                True ->
                    anagramHelper
                        (removeChar p orig)
                        True
                        ps

        _ ->
            False


removeChar : Char -> List Char -> List Char
removeChar char s =
    case s of
        [] ->
            []

        a :: rest ->
            case a == char of
                True ->
                    rest

                False ->
                    a :: removeChar char rest

