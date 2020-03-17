{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module WordCount
  ( wordCount
  ) where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Char            as C
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import qualified Data.Text            as T

wordCount :: Text -> Map Text Int
wordCount xs = foldr g M.empty ws
  where
    ws :: [Text]
    ws = fmap T.toLower (cleanWords xs)
    
    f :: Maybe Int -> Maybe Int
    f Nothing  = Just 1
    f (Just v) = Just (v + 1)

    g :: Text -> Map Text Int -> Map Text Int
    g t m = M.alter f t m

cleanWords :: Text -> [Text]
cleanWords t =
  let p :: Parser [Text]
      p = A.skipWhile (not . C.isAlphaNum) >> parseWords
   in case A.parseOnly p t of
        Left _  -> []
        Right a -> a

parseWords :: Parser [Text]
parseWords = do
  A.sepBy parseWord (A.skipWhile (not . C.isAlphaNum))

{- A word will always be one of:
A number composed of one or more ASCII digits 
(ie "0" or "1234") 
OR
A simple word composed of one or more ASCII letters 
(ie "a" or "they") 
OR
A contraction of two simple words joined by a single 
apostrophe (ie "it's" or "they're") -}

parseWord :: Parser Text
parseWord =
  A.choice
    [ fmap T.pack (A.many1 A.digit)
    , parseContraction
    , fmap T.pack (A.many1 A.letter)
    ]

parseContraction :: Parser Text
parseContraction = do
  start <- A.many1 A.letter
  apostrophe <- A.char '\''
  end <- A.many1 A.letter
  pure (T.pack (start <> [apostrophe] <> end))
