{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module RunLength
  ( decode
  , encode
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as At
import qualified Data.Char            as Char
import           Data.List            (any)
import qualified Data.Text            as Text

data RleCode a =
     Run Int a
     deriving (Show)

decode :: String -> String
decode encodedText =
  let f :: [(RleCode Char)] -> String
      f l =
        case l of
          a:rest ->
            case a of
              Run n x -> replicate n x ++ f rest
          _ -> ""
   in f (toRleCode encodedText)

encode :: String -> String
encode text =
  let f :: [(RleCode Char)] -> String
      f l =
        case l of
          a:rest ->
            case a of
              Run n x ->
                case n of
                  1 -> x : f rest
                  _ -> (show n) ++ x : f rest
          _ -> ""
   in f (toRleCode text)

-- Converts a string to RleCode Char. The string may represent
-- either text that has already been encoded or not.
-- Examples : "QWWE" -> [Run 1 'Q',Run 2 'W',Run 1 'E']
-- "1Q2W1E" -> [Run 1 'Q',Run 2 'W',Run 1 'E']
toRleCode :: String -> [(RleCode Char)]
toRleCode s =
  let f :: (Int, Char) -> [(RleCode Char)] -> [(RleCode Char)]
      f = (\t acc -> (Run (fst t) (snd t)) : acc)
   in case any (\x -> Char.isDigit x) s of
        False ->
          case At.parseOnly (At.many1' manyChars) (Text.pack s) of
            Left _     -> []
            Right list -> foldr f [] list
        True ->
          case At.parseOnly (At.many1' parseChars) (Text.pack s) of
            Left _     -> []
            Right list -> foldr f [] list

-- Parses one char and returns a tuple indicating
-- that it appears one time.
oneChar :: Parser (Int, Char)
oneChar = do
  x <- At.letter <|> At.space
  pure (1, x)

-- Parses several chars and returns a tuple indicating
-- the amount of times said char appears.
manyChars :: Parser (Int, Char)
manyChars = do
  l <- At.letter <|> At.space
  x <- At.takeWhile (\c -> c == l)
  pure (Text.length x + 1, l)

-- Parses a decimal followed by either a letter
-- or a whitespace.
parseNumChar :: Parser (Int, Char)
parseNumChar = do
  n <- At.decimal
  c <- At.letter <|> At.space
  pure (n, c)

-- Parses either a decimal followed by a letter or whitespace,
-- or just a letter or whitespace. It returns a tuple
-- indicating how many times the char appears.
parseChars :: Parser (Int, Char)
parseChars = parseNumChar <|> oneChar
