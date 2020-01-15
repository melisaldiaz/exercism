module WordCount exposing (..)

import Dict exposing (Dict)



wordCount : String -> Dict String Int
wordCount sentence =
    let words : List String
        words = case sequence (List.map cleanWord (String.words (String.replace "," " " sentence))) of 
                    Nothing -> []
                    Just l -> List.map String.toLower (List.filter (not << String.isEmpty) l)

        f : Maybe Int -> Maybe Int 
        f x =
            case x of 
                Nothing -> Just 1
                Just v -> Just (v + 1)
        
        g : String -> Dict String Int -> Dict String Int 
        g s d = Dict.update s f d 
    in
    List.foldr g Dict.empty words


cleanWord : String -> Maybe String 
cleanWord w =
    case w of 
        "" -> Nothing
        _ -> Just (String.filter (not << isPunctuation) (apostrophe w))



apostrophe : String -> String 
apostrophe s =
    case String.contains "n't" s
        || String.contains "'s" s
        || String.contains "s'" s
     of
        True -> s 
        False -> String.fromList (removeChar '\'' (String.toList s))
    


isPunctuation : Char -> Bool
isPunctuation c =
    case c of
        '.' -> True
        ',' -> True
        '?' -> True
        '!' -> True
        ':' -> True
        '&' -> True 
        '@' -> True 
        '$' -> True 
        '%' -> True 
        '^' -> True
        _ -> False



removeChar : Char -> List Char -> List Char
removeChar char s =
    case s of
        [] ->
            []

        a :: rest ->
            case a == char of
                True ->
                    removeChar char rest

                False ->
                    a :: removeChar char rest



sequence : List (Maybe x) -> Maybe (List x)
sequence lm =
    case lm of
        [] ->
            Just []

        a :: rest ->
            Maybe.map2 (::) a (sequence rest)
