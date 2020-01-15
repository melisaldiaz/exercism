module Transpose exposing (transpose)


transpose : List String -> List String
transpose lines =
    let regularLines : List String
        regularLines = let l : Int 
                           l = longestStringLength lines 
                        in List.map (String.padRight l ' ') (List.map (String.replace " " "/") lines) 
    in List.map (String.replace "/" " ") (List.map String.trimRight (transposeRegular regularLines))


transposeRegular : List String -> List String
transposeRegular lines =
    case sequence (List.map String.uncons lines) of 
        Just [] -> []
        Nothing -> []
        Just l -> case List.unzip l of 
                    (firsts,rests) -> String.fromList firsts :: transposeRegular rests 


longestStringLength : List String -> Int  
longestStringLength list =
    case List.maximum (List.map (String.length) list) of 
        Nothing -> 0
        Just l -> l 


sequence : List (Maybe x) -> Maybe (List x)
sequence lm =
    case lm of
        [] ->
            Just []

        a :: rest ->
            Maybe.map2 (::) a (sequence rest)