module Gigasecond exposing (..)

import Time


gs_in_seconds : Int 
gs_in_seconds = 10 ^ 9

gs_in_millis : Int 
gs_in_millis = gs_in_seconds * 1000


add : Time.Posix -> Time.Posix
add timestamp =
    Time.millisToPosix ((Time.posixToMillis timestamp) + gs_in_millis)