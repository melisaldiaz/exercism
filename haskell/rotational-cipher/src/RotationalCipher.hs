module RotationalCipher (rotate) where

import Data.Char (toUpper)

rotate :: Int -> String -> String
rotate rot msg = translate msg (encodeAbc rot)

abc :: String
abc = "abcdefghijklmnopqrstuvwxyz"

encodeAbc :: Int -> String
encodeAbc 0 = abc
encodeAbc 26 = abc
encodeAbc n = drop n abc ++ take n abc

translate ::
  String -> -- to translate
  String -> -- ciphered alphabet
  String
translate x calph =
  let newAbc :: [(Char, Char)]
      newAbc =
        zip
          (abc ++ fmap toUpper abc)
          (calph ++ fmap toUpper calph)
   in unwords (fmap (counterpart newAbc) (words x))

counterpart :: [(Char, Char)] -> String -> String
counterpart m plain = foldr f "" plain
  where
    f :: Char -> String -> String
    f c acc = case lookup c m of
      Nothing -> c : acc
      Just x -> x : acc
