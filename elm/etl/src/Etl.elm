module Etl exposing (..)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform input =
    List.foldl Dict.union
        Dict.empty
        (List.map (uncurry tag) (Dict.toList input))


tag : Int -> List String -> Dict String Int
tag n =
    List.map (\s -> ( String.toLower s, n ))
        >> Dict.fromList


uncurry : (a -> b -> c) -> (( a, b ) -> c)
uncurry f =
    \x -> f (Tuple.first x) (Tuple.second x)