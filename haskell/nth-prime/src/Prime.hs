module Prime (nth) where

import Data.List.Ordered (minus)

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | n >= 1 && n <= 5 = Just $ [2, 3, 5, 7, 11] !! (n - 1)
  | otherwise = Just $ fromIntegral $ primesUpTo (n * div n 2) !! (n - 1)

-- Sieve of Eratosthenes
primesUpTo :: Int -> [Int]
primesUpTo n = f [2 .. n]
  where
    f :: [Int] -> [Int]
    f [] = []
    f (x : xs) =
      x : f (minus xs [x * x, x * x + x .. n])
