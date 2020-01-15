module RunLengthEncoding exposing (encode,decode)


type RleCode a
    = Run Int a
    | Single a

encode : String -> String
encode string =
     fromRleCodeE (toRleCode (String.toList string)) 
    

decode : String -> String
decode string = 
    fromRleCodeD (toRleCode (String.toList string)) 

                     

toRleCode : List Char -> List (RleCode Char)
toRleCode list = 
    case list of 
        [] -> []
        a :: rest -> case Char.isDigit a of 
                        False -> case span list (\x -> x == a) of 
                                    (pre, pos) -> case List.length pre of 
                                                    1 -> Single a :: toRleCode pos 
                                                    n -> Run n a :: toRleCode pos
                        True -> case span list (\x -> Char.isDigit x) of
                                    (digits, rest2) -> let num : Int 
                                                           num = Maybe.withDefault 0 (intFromList (Maybe.withDefault [] (sequence (List.map charToInt digits))))
                                                           
                                                           head: Char 
                                                           head = (Maybe.withDefault ' ' (List.head rest2))
                                                       in 
                                                       case num of 
                                                       1 -> Single head :: toRleCode (List.drop 1 rest2)
                                                       _ -> Run num head :: toRleCode (List.drop 1 rest2)
                        

fromRleCodeE : List (RleCode Char) -> String 
fromRleCodeE list = 
    case list of 
        [] -> ""
        a :: rest -> 
            case a of 
              Single x -> String.cons x (fromRleCodeE rest) 
              Run n x -> (String.fromInt n) ++ (String.cons x (fromRleCodeE rest)) 



fromRleCodeD : List (RleCode Char) -> String
fromRleCodeD list = 
    case list of 
        [] -> ""
        a :: rest -> 
            case a of
              Single x -> String.cons x (fromRleCodeD rest)
              Run n x -> String.repeat n (String.fromChar x) ++ fromRleCodeD rest





span : List a -> (a -> Bool) -> ( List a, List a )
span la p =
    ( takeWhile p la, dropWhile p la )


takeWhile : (a -> Bool) -> List a -> List a
takeWhile =
    \predicate list ->
        case list of
            [] ->
                []

            a :: rest ->
                case predicate a of
                    False ->
                        []

                    True ->
                        a :: takeWhile predicate rest


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        a :: rest ->
            case predicate a of
                False ->
                    list

                True ->
                    dropWhile predicate rest


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


intFromList : List Int -> Maybe Int 
intFromList list =
    let length = List.length list
    in
    case list of 
        [] -> Nothing 
        a :: rest -> 
            Just (a * (10^ (length - 1)) + Maybe.withDefault 0 (intFromList rest))
    

sequence : List (Maybe x) -> Maybe (List x)
sequence lm =
    case lm of
        [] ->
            Just []

        a :: rest ->
            Maybe.map2 (::) a (sequence rest)