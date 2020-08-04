module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Control.Monad (replicateM)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (maybe)
import System.Random (randomRIO)

caesarDecode :: String -> String -> String
caesarDecode key encodedText =
  foldr f [] (zip (cycle key) encodedText)
  where
    f :: (Char, Char) -> [Char] -> [Char]
    f (a, b) acc = maybe [] pure (decodeChar a b) ++ acc

caesarEncode :: String -> String -> String
caesarEncode key text =
  foldr f [] (zip (cycle key) text)
  where
    f :: (Char, Char) -> [Char] -> [Char]
    f (a, b) acc = maybe [] pure (encodeChar a b) ++ acc

-- Given a text to encode, encode it with a random key, and
-- return the key and the encoded text.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- getKey
  pure (key, caesarEncode key text)
  where
    getKey :: IO [Char]
    getKey = replicateM (length text) (randomRIO ('a', 'z'))

-- Map from letters of the alphabet to positions.
alphabet :: Map Char Int
alphabet = M.fromList xs
  where
    xs :: [(Char, Int)]
    xs = zip ['a' .. 'z'] [0 ..]

-- Map from positions to letters of the alphabet.
positions :: Map Int Char
positions = M.fromList xs
  where
    xs :: [(Int, Char)]
    xs = zip [0 ..] ['a' .. 'z']

-- Given a character, find its index in the alphabet, if possible.
toIndex :: Char -> Maybe Int
toIndex char = M.lookup char alphabet

-- Given a Int, return the Char with that index, if possible.
fromIndex :: Int -> Maybe Char
fromIndex nat = M.lookup nat positions

-- Given a Int representing a key, and another Int to shift/unshift,
-- return the corresponding shifted/unshifted Int.
shiftIndex, unshiftIndex :: Int -> Int -> Int
shiftIndex key input = mod (input + key) 26
unshiftIndex key input =
  let x = (mod input 26) - key
   in if x < 0 then x + 26 else x

-- Given a Char as a key and a Char as input, encode/decode the input.
encodeChar, decodeChar :: Char -> Char -> Maybe Char
encodeChar key input =
  fromIndex $ shiftIndex a b
  where
    a , b :: Int
    a = maybe 0 id (toIndex key)
    b = maybe 0 id (toIndex input)
decodeChar key input =
  fromIndex $ unshiftIndex a b
  where
    a , b :: Int
    a = maybe 0 id (toIndex key)
    b = maybe 0 id (toIndex input)
