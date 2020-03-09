module Proverb
  ( recite
  ) where

import           Data.List (intersperse)

recite :: [String] -> String
recite [] = ""
recite l@(x:_) =
  let f :: [String] -> [String] -> [String]
      f [] _           = []
      f [_] acc        = acc
      f (a:b:rest) acc = f (b : rest) (acc ++ [getVerse a b])
   in concat (intersperse "\n" (f l [] ++ [single x]))

single :: String -> String
single x = "And all for the want of a " ++ x ++ "."

getVerse :: String -> String -> String
getVerse a b = prefix ++ a ++ " the " ++ b ++ suffix
  where
    prefix = "For want of a "
    suffix = " was lost."
