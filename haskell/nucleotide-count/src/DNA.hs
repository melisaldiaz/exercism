{-# LANGUAGE LambdaCase #-}

module DNA
  ( nucleotideCounts
  , Nucleotide(..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Nucleotide
  = A
  | C
  | G
  | T
  deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts "" = Right initialDNA
nucleotideCounts (a:rest) =
  case toNucleotide a of
    Nothing -> Left "Not a nucleotide."
    Just x  -> fmap (\m -> updateCount m x) (nucleotideCounts rest)

initialDNA :: Map Nucleotide Int
initialDNA = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

toNucleotide :: Char -> Maybe Nucleotide
toNucleotide =
  \case
    'A' -> Just A
    'C' -> Just C
    'G' -> Just G
    'T' -> Just T
    _ -> Nothing

updateCount :: Map Nucleotide Int -> Nucleotide -> Map Nucleotide Int
updateCount mni n = Map.update f n mni
  where
    f :: Int -> Maybe Int
    f = \x -> Just (x + 1)
