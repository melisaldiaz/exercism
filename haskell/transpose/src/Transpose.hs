module Transpose (transpose) where

import Data.List (uncons)

data Padded = Pad | Orig Char
  deriving (Show, Eq)

transpose :: [[Char]] -> [[Char]]
transpose lines =
  fmap fromPadded (fmap dropPadRight (transpose' ls))
  where
    ls :: [[Padded]]
    ls = padRight (fmap toPadded lines)

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' list =
  case sequence (fmap uncons list) of
    Nothing -> []
    Just listOfTuples ->
      case unzip listOfTuples of
        (heads, tails) -> heads : transpose' tails

-- Convert an unprocessed String into a list
-- containing only Orig chars.
toPadded :: [Char] -> [Padded]
toPadded xs = fmap Orig xs

-- Pad each list of Padded elements in the larger list
-- based on the longest [Padded] length.
padRight :: [[Padded]] -> [[Padded]]
padRight xs = fmap f xs
  where
    n = maximum $ fmap length xs
    f :: [Padded] -> [Padded]
    f x = take n $ x ++ repeat Pad

fromPadded :: [Padded] -> [Char]
fromPadded [] = ""
fromPadded (x : xs) = case x of
  Pad -> " " ++ fromPadded xs
  Orig c -> c : fromPadded xs

-- Drop all occurrences of Pad from the end of the list.
dropPadRight :: [Padded] -> [Padded]
dropPadRight = foldr f []
  where
    f :: Padded -> [Padded] -> [Padded]
    f x xs =
      if x == Pad && null xs
        then []
        else x : xs
