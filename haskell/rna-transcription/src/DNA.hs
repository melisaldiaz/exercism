{-# LANGUAGE LambdaCase #-}

module DNA
  ( toRNA
  ) where

data Nucleotide
  = Adenine
  | Cytosine
  | Guanine
  | Thymine
  | Uracil

toRNA :: String -> Either Char String
toRNA xs =
  case sequence (fmap toNucleotideEi xs) of
    Left x   -> Left x
    Right ns -> Right (fmap (fromNucleotide . complement) ns)

fromNucleotide :: Nucleotide -> Char
fromNucleotide =
  \case
    Adenine -> 'A'
    Cytosine -> 'C'
    Guanine -> 'G'
    Thymine -> 'T'
    Uracil -> 'U'

toNucleotide :: Char -> Maybe Nucleotide
toNucleotide =
  \case
    'A' -> Just Adenine
    'C' -> Just Cytosine
    'G' -> Just Guanine
    'T' -> Just Thymine
    _ -> Nothing

toNucleotideEi :: Char -> Either Char Nucleotide
toNucleotideEi c = maybe (Left c) Right (toNucleotide c)

complement :: Nucleotide -> Nucleotide
complement =
  \case
    Guanine -> Cytosine
    Cytosine -> Guanine
    Thymine -> Adenine
    Adenine -> Uracil
    Uracil -> Adenine
