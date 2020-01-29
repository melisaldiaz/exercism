{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module ResistorColors
  ( Color(..)
  , value
  ) where

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
  deriving (Eq, Show)

value :: (Color, Color) -> Int
value (a, b) = (getVal a * 10) + getVal b

getVal :: Color -> Int
getVal =
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
