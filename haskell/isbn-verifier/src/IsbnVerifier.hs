{-# LANGUAGE OverloadedStrings #-}

module IsbnVerifier
  ( isbn
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Text            (Text)
import qualified Data.Text            as Text

newtype ISBN =
  ISBN Text

-- | Determines whether a string contains a
-- valid ISBN number.
isbn :: String -> Bool
isbn t =
  case makeISBN (Text.pack t) of
    Nothing -> False
    Just _  -> True

-- | Given a text, it parses an ISBN, if possible.
makeISBN :: Text -> Maybe ISBN
makeISBN t =
  case A.parseOnly (isbnRaw <* A.endOfInput) t of
    Left _ -> Nothing
    Right i ->
      case join (fmap checksum (sequence (fmap value (Text.unpack i)))) of
        Just 0 -> Just (ISBN t)
        _      -> Nothing

-- Parses a raw ISBN, with or without hyphens.
isbnRaw :: Parser Text
isbnRaw = noHyphens <|> withHyphens

-- Parses 9 digits and a last char that may be
-- another digit or an 'X'.
noHyphens :: Parser Text
noHyphens = do
  n <- nineDigits
  x <- tenth
  pure (n <> Text.pack [x])

-- Parses a hyphenated number, with the following
-- format : N-NNN-NNNNN-X. The tenth char may be
-- a digit or an 'X'.
withHyphens :: Parser Text
withHyphens = do
  a <- A.digit
  _ <- aHyphen
  b <- replicateM 3 A.digit
  _ <- aHyphen
  c <- replicateM 5 A.digit
  _ <- aHyphen
  d <- tenth
  pure (Text.pack ([a] <> b <> c <> [d]))

-- Parses a Char that may be an 'X' or a digit.
tenth :: Parser Char
tenth = anX <|> A.digit

-- Parses an 'X'.
anX :: Parser Char
anX = A.char 'X'

-- Parses a hyphen.
aHyphen :: Parser Char
aHyphen = A.char '-'

-- Parses nine digits.
nineDigits :: Parser Text
nineDigits = 
    fmap Text.pack (replicateM 9 A.digit)

-- Calculates the validity of the possible ISBN.
checksum :: [Int] -> Maybe Int
checksum list =
  case list of
    a:b:c:d:e:f:g:h:i:j:[] ->
      Just
        (mod
           (a * 10 + b * 9 + c * 8 + d * 7 + e * 6 + f * 5 + g * 4 + h * 3 + i * 2 + j * 1)
           11)
    _ -> Nothing

value :: Char -> Maybe Int
value n =
  case n of
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
    'X' -> Just 10
    _   -> Nothing
