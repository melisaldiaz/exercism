{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Atbash
  ( decode
  , encode
  ) where

import           Data.Char       as C
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T

decode :: Text -> Text
decode cipherText = T.map getOpposite c
  where
    c :: Text
    c = T.replace " " "" cipherText

encode :: Text -> Text
encode plainText = T.map getOpposite (clean plainText)
  where
    clean :: Text -> Text
    clean = T.intercalate " " 
          . T.chunksOf 5 
          . T.filter C.isAlphaNum 
          . T.toLower

getOpposite :: Char -> Char
getOpposite c = fromMaybe c (M.lookup c mAbc)

mAbc :: Map Char Char
mAbc = M.fromList (T.zip abc (T.reverse abc))

abc :: Text
abc = "abcdefghijklmnopqrstuvwxyz"
