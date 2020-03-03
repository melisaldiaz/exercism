module Triangle
  ( rows
  ) where

rows :: Int -> [[Integer]]
rows n
  | n < 1 = []
  | otherwise = rows (n - 1) ++ c
  where
    c :: [[Integer]]
    c = [fmap fromIntegral (fmap (cell (n - 1)) [0 .. (n - 1)])]

cell :: Int -> Int -> Int
cell row col = 
    div (factorial row) (factorial col * factorial (row - col))

factorial :: Int -> Int
factorial n = product [1 .. n]
