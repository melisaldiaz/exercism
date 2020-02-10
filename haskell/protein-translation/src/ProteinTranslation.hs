{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProteinTranslation
  ( proteins
  ) where

import           Data.Text  (Text, chunksOf, pack, unpack)

data AminoAcid
  = Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  deriving (Eq, Show)

data Codon
  = AUG
  | UUU
  | UUC
  | UUA
  | UUG
  | UCU
  | UCC
  | UCA
  | UCG
  | UAU
  | UAC
  | UGU
  | UGC
  | UGG
  | UAA
  | UAG
  | UGA

proteins :: String -> Maybe [String]
proteins s = do
  let chunks :: [Text] = chunksOf 3 (pack s)
  codons :: [Codon] <- traverse toCodon chunks
  aacids :: [AminoAcid] <-
    traverse fromCodon (takeWhile (not . isStopCodon) codons)
  pure (fmap (unpack . fromAminoAcid) aacids)

fromCodon :: Codon -> Maybe AminoAcid
fromCodon =
  \case
    AUG -> Just Methionine
    UUU -> Just Phenylalanine
    UUC -> Just Phenylalanine
    UUA -> Just Leucine
    UUG -> Just Leucine
    UCU -> Just Serine
    UCC -> Just Serine
    UCA -> Just Serine
    UCG -> Just Serine
    UAU -> Just Tyrosine
    UAC -> Just Tyrosine
    UGU -> Just Cysteine
    UGC -> Just Cysteine
    UGG -> Just Tryptophan
    _ -> Nothing

toCodon :: Text -> Maybe Codon
toCodon =
  \case
    "AUG" -> Just AUG
    "UUU" -> Just UUU
    "UUC" -> Just UUC
    "UUA" -> Just UUA
    "UUG" -> Just UUG
    "UCU" -> Just UCU
    "UCC" -> Just UCC
    "UCA" -> Just UCA
    "UCG" -> Just UCG
    "UAU" -> Just UAU
    "UAC" -> Just UAC
    "UGU" -> Just UGU
    "UGC" -> Just UGC
    "UGG" -> Just UGG
    "UAA" -> Just UAA
    "UAG" -> Just UAG
    "UGA" -> Just UGA
    _ -> Nothing

isStopCodon :: Codon -> Bool
isStopCodon UAA = True
isStopCodon UAG = True
isStopCodon UGA = True
isStopCodon _   = False

fromAminoAcid :: AminoAcid -> Text
fromAminoAcid =
  \case
    Methionine -> "Methionine"
    Phenylalanine -> "Phenylalanine"
    Leucine -> "Leucine"
    Serine -> "Serine"
    Tyrosine -> "Tyrosine"
    Cysteine -> "Cysteine"
    Tryptophan -> "Tryptophan"
