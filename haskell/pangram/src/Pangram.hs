module Pangram
  ( isPangram
  ) where

import           Data.Char (toLower)
import           Data.Set  (Set)
import qualified Data.Set  as Set

isPangram :: String -> Bool
isPangram text = 
    Set.null (Set.difference alphabet sentence)
  where
    sentence :: Set Char
    sentence = Set.fromList (fmap toLower text)

alphabet :: Set Char
alphabet =
  Set.fromList "abcdefghijkmlnopqrstuvwxyz"


