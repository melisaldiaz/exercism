{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Allergies
  ( Allergen(..)
  , allergies
  , isAllergicTo
  ) where

import           Data.Bits ((.&.))

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Eq, Show)

allergies :: Int -> [Allergen]
allergies score = 
    filter (\a -> isAllergicTo a score) listOfAllergies

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = 
    (allergyMask allergen) .&. score /= 0

allergyMask :: Allergen -> Int
allergyMask =
  \case
    Eggs -> 1
    Peanuts -> 2
    Shellfish -> 4
    Strawberries -> 8
    Tomatoes -> 16
    Chocolate -> 32
    Pollen -> 64
    Cats -> 128

listOfAllergies :: [Allergen]
listOfAllergies =
  [Eggs
  , Peanuts
  , Shellfish
  , Strawberries
  , Tomatoes
  , Chocolate
  , Pollen
  , Cats]
