module Squares
  ( difference
  , squareOfSum
  , sumOfSquares
  ) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = sqr (sumN n)

sumOfSquares :: Integral a => a -> a
sumOfSquares n = div ((n * (n + 1)) * (2 * n + 1)) 6

sqr :: Integral a => a -> a
sqr n = n * n

-- | Sum of the first n natural numbers.
sumN :: Integral a => a -> a
sumN n = div (n * (n + 1)) 2
