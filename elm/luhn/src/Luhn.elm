module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let i : String
        i = String.filter (\a -> a /= ' ') input
    in 
    case String.length i > 1 && String.all Char.isDigit i of 
        False -> False 
        True -> let processedNum : List Int  
                    processedNum = doubling (stringToDigits (String.reverse i))
                in  
                even (modBy 10 (List.sum processedNum))

-- Doubles every second digit
doubling : List Int -> List Int 
doubling s =
    case s of 
        [] -> []
        a :: b :: rest -> if (b * 2) > 9 then 
                             a :: (b * 2) - 9 :: doubling rest
                          else a :: (b * 2) :: doubling rest
        _ -> []


stringToDigits : String -> List Int
stringToDigits s =
    case s of
        "" -> []

        _ -> let
                chars : List Char
                chars = String.toList s
             in
             case sequence (List.map charToInt chars) of
                Nothing -> []
                Just x -> x


sequence : List (Maybe x) -> Maybe (List x)
sequence lm =
    case lm of
        [] -> Just []
        a :: rest -> Maybe.map2 (::) a (sequence rest)


charToInt : Char -> Maybe Int
charToInt c =
    case c of
        '0' -> Just 0
        '1' -> Just 1
        '2' -> Just 2
        '3' -> Just 3
        '4' -> Just 4
        '5' -> Just 5
        '6' -> Just 6
        '7' -> Just 7
        '8' -> Just 8
        '9' -> Just 9
        _ ->  Nothing


even : Int -> Bool
even =
    \x -> modBy 2 x == 0

