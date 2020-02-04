module Scrabble
  ( scoreLetter
  , scoreWord
  ) where

import           Data.Char       (toUpper)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

scoreLetter :: Char -> Integer
scoreLetter letter =
  fromIntegral (Map.findWithDefault 0 (toUpper letter) (makeDict scores))

scoreWord :: String -> Integer
scoreWord word = fromIntegral (sum (fmap g w))
  where
    w :: String
    w = fmap toUpper word
    g :: Char -> Int
    g c = Map.findWithDefault 0 c (makeDict scores)

scores :: [([Char], Int)]
scores =
  [ (['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'], 1)
  , (['D', 'G'], 2)
  , (['B', 'C', 'M', 'P'], 3)
  , (['F', 'H', 'V', 'W', 'Y'], 4)
  , (['K'], 5)
  , (['J', 'X'], 8)
  , (['Q', 'Z'], 10)
  ]

makeDict :: [([Char], Int)] -> Map Char Int
makeDict values = Map.fromList (concat (fmap g values))
  where
    g :: ([Char], Int) -> [(Char, Int)]
    g x = fmap (\l -> (l, snd x)) (fst x)
