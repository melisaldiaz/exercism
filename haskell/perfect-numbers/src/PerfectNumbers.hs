module PerfectNumbers
  ( classify
  , Classification(..)
  ) where

import qualified Data.Maybe as Maybe

-- The aliquot sums of perfect, deficient, and abundant
-- numbers are equal to, less than, and greater than the
-- number itself respectively.
data Classification
  = Deficient
  | Perfect
  | Abundant
  deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | aliquotsum == n = Just Perfect
  | aliquotsum > n = Just Abundant
  | otherwise = Just Deficient
  where
    aliquotsum = (aliquotSum n)

--  The aliquot sum s(n) of a positive integer n is the
-- sum of all proper divisors of n, that is, all divisors
-- of n other than n itself.
aliquotSum :: Int -> Int
aliquotSum n = sum (aliquots n)

-- Enumerates all the proper divisors of a positive integer n.
aliquots :: Int -> [Int]
aliquots n = Maybe.catMaybes (fmap f [1 .. (div n 2)])
  where
    f :: Int -> Maybe Int
    f x =
      if mod n x == 0
        then Just x
        else Nothing
