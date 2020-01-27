module Isogram
  ( isIsogram
  ) where

import Data.Char (isAlpha, toLower)
import Data.List (sort,nub)

isIsogram :: String -> Bool
isIsogram =
    allDifferent .
    fmap toLower .
    filter isAlpha

allDifferent :: String -> Bool
allDifferent s = nub s == s
