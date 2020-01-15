module Triangle exposing (..)


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    if x <= 0 || y <= 0 || z <= 0 then
        Err "Invalid lengths"

    else if not (inequality x y z) then
        Err "Violates inequality"

    else if x == y && y == z then
        Ok Equilateral

    else if x == y || y == z || x == z then
        Ok Isosceles

    else
        Ok Scalene

-- Given the lengths of the sides of a triangle, it determines if the triangle inequality holds, 
--i.e., if the sum of the lengths of any two sides is greater than or equal to the length of the remaining side.
inequality : number -> number -> number -> Bool
inequality x y z =
    (z + y) >= x && (x + z) >= y && (x + y) >= z
