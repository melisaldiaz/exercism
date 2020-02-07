{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module SecretHandshake
  ( handshake
  ) where

import           Control.Monad (guard)
import           Data.Bits (testBit)
import           Data.Functor  (($>), (<$))
import           Data.Maybe    (catMaybes)

handshake :: Int -> [String]
handshake n
  | testBit n 4 = reverse xs
  | otherwise = xs
  where
    xs :: [String]
    xs =
      catMaybes
        [ guard (testBit n 0) $> "wink"
        , "double blink" <$ guard (testBit n 1)
        , guard (testBit n 2) *> pure "close your eyes"
        , if testBit n 3
            then Just "jump"
            else Nothing
        ]
