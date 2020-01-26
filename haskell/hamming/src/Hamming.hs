module Hamming
  ( distance
  ) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance [] _ = Nothing
distance _ [] = Nothing
distance (x:xs) (y:ys)
  | length (x : xs) /= length (y : ys) = Nothing
  | x /= y = Just (maybe 0 (+ 1) (distance xs ys))
  | otherwise = distance xs ys
