module MatchingBrackets exposing (..)


type Bracket
    = RoundBracket
    | CurlyBracket
    | SquareBracket


type Side
    = Opening
    | Closing


isPaired : String -> Bool
isPaired input =
    confirmPairs (String.toList input) []


confirmPairs : List Char -> List Bracket -> Bool
confirmPairs list acc =
    case ( list, acc ) of
        ( [], _ :: _ ) ->
            False

        ( [], [] ) ->
            True

        ( a :: rest, [] ) ->
            case relevantChar a of
                Nothing ->
                    confirmPairs rest acc

                Just ( Opening, b ) ->
                    confirmPairs rest (b :: acc)

                Just ( Closing, b ) ->
                    False

        ( a :: rest, br :: brs ) ->
            case relevantChar a of
                Nothing ->
                    confirmPairs rest acc

                Just ( Opening, b ) ->
                    confirmPairs rest (b :: acc)

                Just ( Closing, b ) ->
                    case b == br of
                        True ->
                            confirmPairs rest brs

                        False ->
                            False


relevantChar : Char -> Maybe ( Side, Bracket )
relevantChar c =
    case c of
        '[' ->
            Just ( Opening, SquareBracket )

        ']' ->
            Just ( Closing, SquareBracket )

        '(' ->
            Just ( Opening, RoundBracket )

        ')' ->
            Just ( Closing, RoundBracket )

        '{' ->
            Just ( Opening, CurlyBracket )

        '}' ->
            Just ( Closing, CurlyBracket )

        _ ->
            Nothing
