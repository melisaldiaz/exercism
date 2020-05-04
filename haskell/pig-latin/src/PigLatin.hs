{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module PigLatin
  ( translate,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Data.Text (Text)

data Sound
  = Vowel
  | Consonant
  | VowelCluster
  | ConsonantCluster

data Onset = Onset Sound Text

translate :: Text -> Text
translate = T.unwords . fmap pig . T.words

pig :: Text -> Text
pig t = mconcat $ case A.parseOnly parseOnset t of
  Left _ -> [""]
  Right o -> case o of
    (Onset Vowel v, x) -> [v, x, "ay"]
    (Onset VowelCluster vc, x) -> [vc, x, "ay"]
    (Onset Consonant c, x) -> [x, c, "ay"]
    (Onset ConsonantCluster cc, x) -> [x, cc, "ay"]

parseOnset :: Parser (Onset, Text)
parseOnset = pv <|> pvc <|> pcc <|> pc

pc :: Parser (Onset, Text)
pc = do
  c <- A.choice (fmap A.string consonants)
  rest <- A.takeText
  pure (Onset Consonant c, rest)

pcc :: Parser (Onset, Text)
pcc = do
  cc <- A.choice (fmap A.string consonantClusters)
  rest <- A.takeText
  pure (Onset ConsonantCluster cc, rest)

pv :: Parser (Onset, Text)
pv = do
  v <- A.choice (fmap A.string vowels)
  rest <- A.takeText
  pure (Onset Vowel v, rest)

pvc :: Parser (Onset, Text)
pvc = do
  vc <- A.choice (fmap A.string vowelClusters)
  rest <- A.takeText
  pure (Onset VowelCluster vc, rest)

vowels, vowelClusters :: [Text]
vowels = ["a", "e", "i", "o", "u"]
vowelClusters = ["yt", "xr"]

consonants, consonantClusters :: [Text]
consonants = ["b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"]
consonantClusters = ["ch", "qu", "rh", "sch", "squ", "thr", "th"]
