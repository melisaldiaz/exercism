module Sieve
  ( primesUpTo
  ) where

import           Data.List.Ordered (minus)

primesUpTo :: Integer -> [Integer]
primesUpTo n = f [2 .. n]
  where
    f :: [Integer] -> [Integer]
    f []     = []
    f (x:xs) = x : f (minus xs [x * x,x * x + x .. n])
