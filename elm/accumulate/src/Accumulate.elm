module Accumulate exposing (accumulate)


accumulate : (a -> b) -> List a -> List b
accumulate func input =
    case input of
        [] ->
            []

        a :: rest ->
            func a :: accumulate func rest
