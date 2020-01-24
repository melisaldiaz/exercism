module SumOfMultiples
  ( sumOfMultiples
  ) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum ms
  where
    ms :: [Integer]
    ms = noDuplicates (concat (fmap (multiples limit) factors))

multiples :: Integer -> Integer -> [Integer]
multiples 0 _ = []
multiples _ 0 = [0]
multiples n factor
  | mod (n - 1) factor == 0 = (n - 1) : multiples (n - 1) factor
  | otherwise = multiples (n - 1) factor

noDuplicates :: Eq a => [a] -> [a]
noDuplicates []       = []
noDuplicates (a:rest) = a : noDuplicates (filter (\x -> x /= a) rest)
