{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module PrimeFactors
  ( primeFactors
  ) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n
  | factors == [] = [n]
  | otherwise =
    case factors of
      a:_ -> factors ++ primeFactors (div n a)
      _   -> []
  where
    factors :: [Integer]
    factors = take 1 (filter f [2 .. (round (sqrt nn))])
      where
        f :: Integer -> Bool
        f x = mod n x == 0
        nn :: Double
        nn = (fromIntegral n)
        
