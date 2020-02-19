module Roman
  ( numerals
  ) where

import           Data.List (foldl')

numerals :: Integer -> Maybe String
numerals n
  | n < 1 || n > 3000 = Nothing
  | otherwise = Just (fst (foldl' f ("", n) nums))
  where
    f :: (String, Integer) -> (Integer, String) -> (String, Integer)
    f (output, n1) (divisor, roman) =
      let qr = quotRem n1 divisor
       in (output ++ concat (replicate (fromIntegral (fst qr)) roman), snd qr)

nums :: [(Integer, String)]
nums =
  [ (1000, "M")
  , (900, "CM")
  , (500, "D")
  , (400, "CD")
  , (100, "C")
  , (90, "XC")
  , (50, "L")
  , (40, "XL")
  , (10, "X")
  , (9, "IX")
  , (5, "V")
  , (4, "IV")
  , (1, "I")
  ]
