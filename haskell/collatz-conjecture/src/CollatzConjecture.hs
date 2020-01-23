module CollatzConjecture
  ( collatz
  ) where

import           Data.Foldable (length)

collatz :: Integer -> Maybe Integer
collatz n
  | n > 0 = Just (toInteger (length (stepsList n)))
  | otherwise = Nothing

step :: Integer -> Integer
step n =
  if even n
    then div n 2
    else (n * 3) + 1

stepsList :: Integer -> [Integer]
stepsList 1 = []
stepsList n = n : stepsList (step n)
