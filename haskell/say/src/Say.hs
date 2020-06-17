{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Say
  ( inEnglish,
  )
where

import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 = Nothing
  | n == 0 = Just "zero"
  | otherwise =
    let digits = toDigits n
        groups :: [[Integer]]
        groups = reverse $ fmap reverse $ chunksOf 3 $ reverse digits
     in Just $ scaling $ fmap groupsOfThree groups

-----------------------------------------------------------------------

-- Given a list of one to three elements it returns the
-- number written in words.
-- Example :: [1,3,0] -> "one hundred thirty"
groupsOfThree :: [Integer] -> String
groupsOfThree xs =
  case xs of
    third : [] -> units [third]
    second : third : [] -> tens [second, third]
    first : second : third : [] -> hundreds [first, second, third]
    _ -> ""

-----------------------------------------------------------------------

-- How to deal with units, tens and hundreds.

units :: [Integer] -> String
units [x] = fromMaybe "" (M.lookup x numWordAssoc)
units _ = ""

tens :: [Integer] -> String
tens (x : y : [])
  | x >= 1 && y == 0 = tyWord
  | x >= 2 && y >= 1 = tyWord ++ "-" ++ units [y]
  | x >= 1 && y >= 1 = fromMaybe "" (M.lookup teen numWordAssoc)
  | x == 0 && y >= 1 = units [y]
  | otherwise = ""
  where
    ty :: Integer
    ty = fromMaybe 0 (fromDigits [x, 0])
    tyWord :: String
    tyWord = fromMaybe "" (M.lookup ty numWordAssoc)
    teen :: Integer
    teen = fromMaybe 0 (fromDigits [x, y])
tens _ = ""

hundreds :: [Integer] -> String
hundreds (x : y : z : [])
  | x == 0 && y == 0 && z == 0 = ""
  | x == 0 && y == 0 = units [z]
  | x == 0 = tens [y, z]
  | otherwise =
    if rem n 100 == 0
      then amount ++ " hundred"
      else amount ++ " hundred " ++ tens [y, z]
  where
    n :: Integer
    n = fromMaybe 0 (fromDigits [x, y, z])
    amount :: String
    amount = fromMaybe "" (M.lookup (div n 100) numWordAssoc)
hundreds _ = ""

-----------------------------------------------------------------------

-- Given a list of written numbers of upto hundreds, it returns a string with the
-- number written in English, adding the corresponding large number names.
-- Example: scaling ["one","two","three","four"] -> "one billion two million three thousand four"
scaling :: [String] -> String
scaling [] = ""
scaling list@(a : _) =
  let indexedL :: [(Integer, String)]
      indexedL = zip [0 ..] $ reverse list
   in if length list == 1
        then a
        else
          mconcat
            $ intersperse " "
            $ reverse
            $ mconcat
            $ fmap largeNumbers indexedL

-- Given a tuple, whose first element is its index in a bigger list, and whose
-- second element is a written number, it returns a reversed list of the
-- written number plus its large number name according to its index.
-- Example: (3, "one hundred ten") -> ["billion","one hundred ten"]

largeNumbers :: (Integer, String) -> [String]
largeNumbers (index, inWords) =
  let w = null inWords
      f :: Maybe [String]
      f = case index of
        0 -> if w then Just [] else Just [inWords]
        1 -> if w then Just [] else Just ["thousand", inWords]
        2 -> if w then Just [] else Just ["million", inWords]
        3 -> if w then Just [] else Just ["billion", inWords]
        _ -> Nothing
   in fromMaybe [] f

-----------------------------------------------------------------------

-- Deconstruct an Integer into a list of its digits.
toDigits :: Integer -> [Integer]
toDigits n
  | n >= 0 && n < 10 = [n]
  | otherwise = toDigits (div n 10) ++ [mod n 10]

-- Given a list of digits, reconstruct the original Integer, if possible.
fromDigits :: [Integer] -> Maybe Integer
fromDigits [] = Nothing
fromDigits digits@(x : xs) =
  Just $ (x * (10 ^ (length digits - 1))) + fromMaybe 0 (fromDigits xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  case splitAt n xs of
    (a, b)
      | null a -> []
      | otherwise -> a : (chunksOf n b)

-----------------------------------------------------------------------

numWordAssoc :: M.Map Integer String
numWordAssoc =
  M.fromList
    [ (1, "one"),
      (2, "two"),
      (3, "three"),
      (4, "four"),
      (5, "five"),
      (6, "six"),
      (7, "seven"),
      (8, "eight"),
      (9, "nine"),
      (10, "ten"),
      (11, "eleven"),
      (12, "twelve"),
      (13, "thirteen"),
      (14, "fourteen"),
      (15, "fifteen"),
      (16, "sixteen"),
      (17, "seventeen"),
      (18, "eighteen"),
      (19, "nineteen"),
      (20, "twenty"),
      (30, "thirty"),
      (40, "forty"),
      (50, "fifty"),
      (60, "sixty"),
      (70, "seventy"),
      (80, "eighty"),
      (90, "ninety")
    ]
