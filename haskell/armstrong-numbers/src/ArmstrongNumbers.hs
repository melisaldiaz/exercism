module ArmstrongNumbers
  ( armstrong
  ) where

import           Numeric.Natural (Natural)

armstrong :: Integral a => a -> Bool
armstrong a
  | a < 0 = False
  | otherwise =
    let digits :: [Natural]
        digits = toDigits (fromIntegral a)
     in fromIntegral a == sum (fmap (\n -> n ^ length digits) digits)

toDigits :: Natural -> [Natural]
toDigits n
  | n >= 0 && n < 10 = [n]
  | otherwise = toDigits (div n 10) ++ [mod n 10]
