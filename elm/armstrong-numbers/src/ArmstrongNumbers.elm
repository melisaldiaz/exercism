module ArmstrongNumbers exposing (..)


isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    let
        x =
            toDigits nb
    in
    nb == List.sum (List.map (flip (^) (List.length x)) x)

-- Digits are returned in reverse order.
toDigits : Int -> List Int
toDigits n =
    case n < 10 of
        True ->
            [ n ]

        False ->
            modBy 10 n :: toDigits (n // 10)


flip : (a -> b -> c) -> (b -> a -> c)
flip f =
    \b a -> f a b
