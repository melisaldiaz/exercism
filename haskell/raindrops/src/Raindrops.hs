module Raindrops (convert) where

convert :: Int -> String
convert n = 
    let x = if mod n 3 == 0 
            then "Pling"
            else ""
        
        y = if mod n 5 == 0
            then "Plang"
            else ""

        z = if mod n 7 == 0
            then "Plong"
            else ""
    
        xyz = x++y++z
    in 
    case null xyz of 
        True -> show n 
        False -> xyz
    
