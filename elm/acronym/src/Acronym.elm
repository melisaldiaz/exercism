module Acronym exposing (..)


abbreviate : String -> String
abbreviate phrase =
    let
        p =
            String.filter
                (isPunctuation >> not)
                (String.replace "-" " " phrase)

        f : List String -> String
        f list =
            case list of
                [] ->
                    ""

                a :: rest ->
                    case String.uncons a of
                        Just ( x, _ ) ->
                            String.toUpper (String.cons x (f rest))

                        Nothing ->
                            ""
    in
    case p of
        "" ->
            ""

        _ ->
            f (String.words p)


isPunctuation : Char -> Bool
isPunctuation c =
    case c of
        '.' ->
            True

        ',' ->
            True

        '?' ->
            True

        '!' ->
            True

        ':' ->
            True

        ';' ->
            True

        '\'' ->
            True

        '(' ->
            True

        ')' ->
            True

        _ ->
            False
