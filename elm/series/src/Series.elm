module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    if String.isEmpty input then 
        Err "series cannot be empty"

    else if size == 0 then 
        Err "slice length cannot be zero"

    else if size < 0 then 
        Err "slice length cannot be negative"
        
    else if size > String.length input then
        Err "slice length cannot be greater than series length" 
        
    else 
        let digits = stringToDigits input in
        Ok (getSlices size digits [])




getSlices : Int -> List Int -> List (List Int) -> List (List Int)
getSlices n s acc =
    case n > List.length s of 
        True -> acc 

        False -> List.take n s :: getSlices n (List.drop 1 s) acc 




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

