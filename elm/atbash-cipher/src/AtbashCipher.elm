module AtbashCipher exposing (..)
import Dict exposing (Dict)

encode : String -> String
encode plain =
    let p : String 
        p = intercalate " " (splitEvery 5 (String.filter Char.isAlphaNum (String.toLower plain)))
    
        f : Char -> Char 
        f char = Maybe.withDefault char (Dict.get char dictCipher)
    in
    String.fromList (List.map f (String.toList p))


decode : String -> String
decode cipher =
    let c : String 
        c = String.filter (\a -> a /= ' ') cipher

        f : Char -> Char 
        f char = Maybe.withDefault char (Dict.get char dictPlain)
    in 
    String.fromList (List.map f (String.toList c))


plainAl : List Char
plainAl = String.toList "abcdefghijklmnopqrstuvwxyz"


cipheredAl : List Char 
cipheredAl = List.reverse plainAl


-- Dictionary of the plain and ciphered alphabets.
-- Example : ['a','b'] -> ['z','y'] -> [(a,z),(b,y)] 
dictCipher : Dict Char Char
dictCipher = 
   Dict.fromList (List.map2 Tuple.pair plainAl cipheredAl)


-- Dictionary of the ciphered and plain alphabets.
-- Example : ['z','y'] -> ['a','b'] -> [(z,a),(y,b)] 
dictPlain : Dict Char Char 
dictPlain =
    Dict.fromList (List.map2 Tuple.pair cipheredAl plainAl)


splitEvery : Int -> String -> List String 
splitEvery n s =
    case s of 
        "" -> []
        _ -> case n >= String.length s of 
                True -> [s]
                False -> String.left n s :: splitEvery n (String.dropLeft n s)


intercalate : String -> List String -> String 
intercalate separator words =
    case words of
        [] -> ""
        [a] -> a
        a :: rest -> a ++ separator ++ intercalate separator rest 


sequence : List (Maybe x) -> Maybe (List x)
sequence lm =
    case lm of
        [] ->
            Just []

        a :: rest ->
            Maybe.map2 (::) a (sequence rest)
