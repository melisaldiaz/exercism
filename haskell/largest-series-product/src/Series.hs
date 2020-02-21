{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Series
  ( Error(..)
  , largestProduct
  ) where

import           Data.Char (isDigit)

data Error
  = InvalidSpan
  | InvalidDigit Char
  deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits =
  case getError size digits of
    Just e -> Left e
    Nothing ->
      if size == 0 || digits == ""
        then Right 1
        else let ds :: [Integer]
                 ds = stringToDigits digits
                 ps :: [[Integer]]
                 ps = permutate size ds []
                 products :: [Integer]
                 products = fmap product ps
              in Right (maximum products)

getError :: Int -> String -> Maybe Error
getError n s
  | n > length s || n < 0 = Just InvalidSpan
  | any nonDigit s =
    case filter nonDigit s of
      []  -> Nothing
      x:_ -> Just (InvalidDigit x)
  | otherwise = Nothing
  where
    nonDigit :: Char -> Bool
    nonDigit = not . isDigit

-- Returns a list of all the permutations of length n
-- of the given list.
permutate :: Int -> [Integer] -> [[Integer]] -> [[Integer]]
permutate n list acc =
  case n > length list of
    True  -> acc
    False -> take n list : permutate n (drop 1 list) acc

-- Given a string allegedly containing only digits,
-- it returns a list of the numbers in the same order.
-- If there is any non digit char, it returns an empty list.
stringToDigits :: String -> [Integer]
stringToDigits [] = []
stringToDigits s =
  case sequence (fmap charToInt s) of
    Nothing -> []
    Just x  -> x

charToInt :: Char -> Maybe Integer
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
