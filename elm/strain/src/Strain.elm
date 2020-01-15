module Strain exposing (..)


keep : (a -> Bool) -> List a -> List a
keep predicate list =
    case list of
        [] ->
            []

        a :: rest ->
            case predicate a of
                True ->
                    a :: keep predicate rest

                False ->
                    keep predicate rest


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    case list of
        [] ->
            []

        a :: rest ->
            case predicate a of
                True ->
                    discard predicate rest

                False ->
                    a :: discard predicate rest


discard2 : (a -> Bool) -> List a -> List a
discard2 predicate list =
    keep (predicate >> not) list