{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Luhn
  ( isValid,
  )
where

import Data.Char

isValid :: String -> Bool
isValid n =
  let nn :: String
      nn = filter (not . isSpace) n
   in case length nn > 1 && all isDigit nn of
        False -> False
        True ->
          let num :: [Int]
              num = doubling (stringToDigits (reverse nn))
           in even (mod (sum num) 10)

doubling :: [Int] -> [Int]
doubling [] = []
doubling (a : b : rest) =
  if (b * 2) > 9
    then a : (b * 2) - 9 : doubling rest
    else a : (b * 2) : doubling rest
doubling _ = []

stringToDigits :: String -> [Int]
stringToDigits "" = []
stringToDigits s =
  case sequence (fmap charToInt s) of
    Nothing -> []
    Just x -> x

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
