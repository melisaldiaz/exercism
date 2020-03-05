module Triplet
  ( tripletsWithSum
  ) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum =
  let n = div sum 2
   in [1 .. n] >>= \a ->
        [(a + 1) .. n] >>= \b ->
          let c = sum - a - b
           in if c ^ 2 == a ^ 2 + b ^ 2
                then [(a, b, c)]
                else []
