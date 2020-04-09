{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module CryptoSquare
  ( encode,
  )
where

import qualified Data.Char as C
import Data.List (intercalate, transpose)

encode :: String -> String
encode xs =
  intercalate " " (organize (normalize xs))

-- The spaces and punctuation are removed from the input
-- and the message is downcased.
normalize :: String -> String
normalize s = fmap C.toLower (filter C.isAlphaNum s)

-- Encodes and organizes the String as a rectangle.
organize :: String -> [String]
organize s =
  let ls = length s
      (c, r) = getDimensions ls
      rectangle = r * c
      chunks =
        if ls == rectangle
          then chunksOfString c s
          else chunksOfString c (s ++ ws)
        where
          ws = concat (replicate (rectangle - ls) " ")
      enc = codedString chunks
      lenc = length enc
   in if lenc == rectangle
        then chunksOfString r enc
        else padRight (rectangle - lenc) (chunksOfString r enc)

-- Adds a single trailing white space to the
-- last n chunks of the given list.
padRight :: Int -> [String] -> [String]
padRight n ls =
  let l = length ls
      diff = l - n
   in if n > l
        then ls
        else case splitAt diff ls of
          (pre, post) -> pre ++ foldr f [] post
            where
              f :: String -> [String] -> [String]
              f "" _ = []
              f a acc = [take (length a) a ++ " "] ++ acc

-- Encodes the String by reading down the
-- columns going left to right.
codedString :: [String] -> String
codedString [] = ""
codedString ls = concat (transpose ls)

-- Calculates the size of the rectangle.
getDimensions :: Int -> (Int, Int)
getDimensions x = (columns, rows)
  where
    columns = ceiling $ sqrt $ fromIntegral x
    rows = ceiling $ fromIntegral x / fromIntegral columns 

-- Splits a String into chunks of size n.
chunksOfString :: Int -> String -> [String]
chunksOfString n s = f s
  where
    f :: String -> [String]
    f x =
      case splitAt n x of
        (a, b)
          | null a -> []
          | otherwise -> a : f b
