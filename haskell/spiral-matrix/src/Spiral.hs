module Spiral (spiral) where

import Data.List (transpose)

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size = mkSpiral size size 1

mkSpiral :: Int -> Int -> Int -> [[Int]]
mkSpiral 0 _ _ = [[]]
mkSpiral height width size =
  [size .. size + width - 1]
    : rot90 (mkSpiral width (height - 1) (size + width))
  where
    rot90 :: [[Int]] -> [[Int]]
    rot90 = map reverse . transpose


-- Algorithm found at https://rosettacode.org/wiki/Spiral_matrix#Haskell
