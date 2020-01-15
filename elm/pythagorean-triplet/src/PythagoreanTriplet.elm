module PythagoreanTriplet exposing (..)


type alias Triplet =
    ( Int, Int, Int )

triplets : Int -> List Triplet
triplets n =
    let n1 = n // 2
    in bind (List.range 1 n1) (\a -> 
       bind (List.range (a + 1) n1) (\b ->
       let c = n - a - b
       in if c ^ 2 == a ^ 2 + b ^ 2 
          then [(a, b, c)]
          else [] )) 
        

bind : List x -> (x -> List y) -> List y
bind xs f = List.concatMap f xs
