module Leap exposing (..)

{-
   on every year that is evenly divisible by 4
   except every year that is evenly divisible by 100
   unless the year is also evenly divisible by 400
-}


isLeapYear : Int -> Bool
isLeapYear year =
    if (modBy 4 year == 0) && (modBy 100 year /= 0) 
    then True 
    else (modBy 400 year == 0)
