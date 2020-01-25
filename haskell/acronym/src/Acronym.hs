{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Acronym
  ( abbreviate
  ) where

import           Data.Char (isPunctuation, isUpper)
import           Data.Text (Text)
import qualified Data.Text as T

abbreviate :: String -> String
abbreviate =
  T.unpack .
  abbr .
  T.words .
  T.strip .
  T.pack .
  filter (not . isPunctuation) .
  splitBy isUpper . 
  removeConsecutive isUpper . 
  replace '-' ' '

-- | Create an abbreviation from a list of words
abbr :: [Text] -> Text
abbr [] = ""
abbr (a:rest) =
  case T.uncons a of
    Nothing     -> ""
    Just (x, _) -> T.toUpper (T.cons x (abbr rest))

-- | Replace every occurrence of a specified Char in a String.
replace :: Char -> Char -> String -> String
replace _ _ "" = ""
replace old new (a:rest) =
  case a == old of
    True  -> new : replace old new rest
    False -> a : replace old new rest

-- | Splits a String wherever the predicate returns True.
-- No characters are dropped from the resulting String.
splitBy :: (Char -> Bool) -> String -> String
splitBy _ "" = ""
splitBy predicate (s:ss) =
  case predicate s of
    True  -> ' ' : s : splitBy predicate ss
    False -> s : splitBy predicate ss

-- | Remove chars from a String when the predicate
-- consecutely returns True.
removeConsecutive :: (Char -> Bool) -> String -> String
removeConsecutive predicate string =
  let f :: (Char -> Bool) -> String -> String -> String
      f _ [] _ = ""
      f _ [a] _ = [a]
      f p (a:b:rest) [] =
        case (p a, p b) of
          (True, True)   -> a : f p rest [a]
          (True, False)  -> a : b : f p (b : rest) [a]
          (False, True)  -> a : b : f p (b : rest) [b]
          (False, False) -> a : b : f p rest []
      f p (a:rest) (prevs:_)
        | p a && p prevs = f p rest [a]
        | otherwise = a : f p rest [a]
   in f predicate string []
