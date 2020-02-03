module Triangle
  ( TriangleType(..)
  , triangleType
  ) where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = 
 if a <= 0 || b <= 0 || c <= 0 then
        Illegal

    else if not (inequality a b c) then
        Illegal

    else if a == b && b == c then
        Equilateral

    else if a == b || b == c || a == c then
        Isosceles

    else
        Scalene

-- Given the lengths of the sides of a triangle,
--it determines if the triangle inequality holds,
--i.e., if the sum of the lengths of any two sides
-- is greater than or equal to the length of the
-- remaining side.
inequality :: (Num a, Ord a) => a -> a -> a -> Bool
inequality x y z = 
    (z + y) >= x && (x + z) >= y && (x + y) >= z
