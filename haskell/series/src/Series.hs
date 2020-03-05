{-# LANGUAGE LambdaCase #-}

module Series
  ( slices
  ) where

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n > length xs = []
  | otherwise =
    let digits :: [Int]
        digits = stringToDigits xs
     in getSlices n digits []

getSlices :: Int -> [Int] -> [[Int]] -> [[Int]]
getSlices n s acc =
  case n > length s of
    True  -> acc
    False -> take n s : getSlices n (drop 1 s) acc

-- Returns a list of the numbers contained in a string
-- in the same order. If there is any non digit char, 
-- it returns an empty list.
stringToDigits :: String -> [Int]
stringToDigits [] = []
stringToDigits s =
  case sequence (fmap charToInt s) of
    Nothing -> []
    Just x  -> x

charToInt :: Char -> Maybe Int
charToInt =
  \case
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    _ -> Nothing
