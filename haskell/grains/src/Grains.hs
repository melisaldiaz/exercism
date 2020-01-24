module Grains
  ( square
  , total
  ) where

import           Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n
  | n > 64 = Nothing
  | n > 0 = Just (2 ^ (n - 1))
  | otherwise = Nothing

total :: Integer
total = sum (fromJust (sequence (fmap square [1 .. 64])))
