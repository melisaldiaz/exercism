{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Diamond
  ( diamond
  ) where

import           Data.Char (toUpper,isAlpha)
import qualified Data.Text as Text

-- | Given a letter, it prints a diamond starting with 'A'
-- with the supplied letter at the widest point.
diamond :: Char -> Maybe [String]
diamond c 
    | isAlpha c = Just (flipV (upperTriangle (fstTriangle c)))
    | otherwise = Nothing


-- | Given an alphabetic Char, it returns the first triangle
-- of the diamond.
fstTriangle :: Char -> [String]
fstTriangle c =
  let 
      chars :: [(Int, Char)]
      chars = indexed (reverse ['A' .. (toUpper c)])
      
      l :: Int
      l = length chars

      addSpaces :: (Int, Char) -> [Char]
      addSpaces (n, a) =
            concat [ replicate n ' '
                   , [a]
                   , replicate (l - n - 1) ' ' ]

      f :: [(Int, Char)] -> [Char] -> [String]
      f [] [] = []
      f [a] [] = [[snd a]]
      f (a:rest) [] = [addSpaces a] ++ f rest (snd a : [])
      f (a:rest) acc =
        case rest of
          [] -> [addSpaces a]
          _  -> addSpaces a : f rest acc
      f _ _ = []

   in f chars []


-- | Given the first triangle, it returns the whole
-- upper triangle of the diamond.
upperTriangle :: [String] -> [String]
upperTriangle s =
    case foldr f [] (zip (reverse s) (flipH s)) of
       x : xs -> r x xs
       [] -> []

    where 
        f :: (String, String) -> [String] -> [String] 
        f ([],_) _ = []
        f (x, y) acc = [init x ++ y] ++ acc

        r :: String -> [String] -> [String]
        r h rest = replaceString h (centerA ((length h) + 1)) (h : rest)
    

-- | Given the first triangle, it returns its mirrored
-- horizontal version.
flipH :: [String] -> [String]
flipH s = fmap reverse (reverse s)


-- | Given the upper triangle, it returns the 
-- whole diamond.
flipV :: [String] -> [String]
flipV s = s ++ tail (reverse s)


-- | Pairs each element of the list with its index.
indexed :: [a] -> [(Int, a)]
indexed a = zip [0 ..] a


-- | Centers the 'A' according to the given length 
-- for the new String.     
centerA :: Int -> String
centerA n = 
    Text.unpack (Text.center (n - 1) ' ' (Text.pack "A"))


-- | Replaces a particular String within a [String]     
replaceString :: String -> String -> [String] -> [String] 
replaceString _ _ [] = []
replaceString old new (a:rest) = 
    case a == old of 
        True -> new : replaceString old new rest
        False -> a : replaceString old new rest
   