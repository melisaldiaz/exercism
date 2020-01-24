module ArmstrongNumbers
  ( armstrong
  ) where

import           Data.Foldable (length)

armstrong :: Integral a => a -> Bool
armstrong a =
  let x :: Integer
      x = (fromIntegral a)
      
      digits :: [Integer]
      digits = toDigits x
   in x == sum (fmap (\n -> n ^ length digits) digits)

toDigits :: Integer -> [Integer]
toDigits n
  | n < 10 = [n]
  | otherwise = toDigits (div n 10) ++ [mod n 10]
