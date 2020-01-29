{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module ResistorColors
  ( Color(..)
  , Resistor(..)
  , label
  , ohms
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor
  { bands :: (Color, Color, Color)
  } deriving (Show)

label :: Resistor -> Text
label resistor
  | x >= 10 ^ 9 = T.pack (show (div x (10 ^ 9))) <> " gigaohms"
  | x >= 10 ^ 6 = T.pack (show (div x (10 ^ 6))) <> " megaohms"
  | x >= 10 ^ 3 = T.pack (show (div x (10 ^ 3))) <> " kiloohms"
  | otherwise = T.pack (show x ++ " ohms")
  where
    x :: Int
    x = ohms resistor

    

ohms :: Resistor -> Int
ohms resistor =
  case bands resistor of
    (a, b, c) -> 
      ((value a * 10) + value b) * 10 ^ value c

value :: Color -> Int
value =
  \case
    Black -> 0
    Brown -> 1
    Red -> 2
    Orange -> 3
    Yellow -> 4
    Green -> 5
    Blue -> 6
    Violet -> 7
    Grey -> 8
    White -> 9
