module Isogram exposing (..)


isIsogram : String -> Bool
isIsogram sentence =
    sentence
            |> String.toList
            |> List.filter Char.isAlpha
            |> List.map Char.toLower
            |> List.sort
            |> allDifferent


allDifferent : List Char -> Bool
allDifferent list =
    case list of
        [] ->
            True

        a :: rest ->
            case rest of
                [] ->
                    True

                b :: rest2 ->
                    case a == b of
                        True ->
                            False

                        False ->
                            allDifferent rest
