{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module FoodChain
  ( song,
  )
where

import Data.Char (toLower)
import Data.List (intercalate)

data Animal
  = Fly
  | Spider
  | Bird
  | Cat
  | Dog
  | Goat
  | Cow
  | Horse
  deriving (Show, Enum)

song :: String
song = intercalate "\n" (fmap stanza [Fly ..])

stanza :: Animal -> String
stanza Horse = start Horse ++ remark Horse
stanza a =
  start a
    ++ remark a
    ++ concat (fmap continuation (reverse (enumFromTo Fly a)))

start :: Animal -> String
start a =
  "I know an old lady who swallowed a "
    ++ animalToString a
    ++ ".\n"

remark :: Animal -> String
remark =
  \case
    Fly -> ""
    Spider ->
      "It wriggled and jiggled and tickled inside her.\n"
    Bird ->
      "How absurd to swallow a bird!\n"
    Cat ->
      "Imagine that, to swallow a cat!\n"
    Dog ->
      "What a hog, to swallow a dog!\n"
    Goat ->
      "Just opened her throat and swallowed a goat!\n"
    Cow ->
      "I don't know how she swallowed a cow!\n"
    Horse ->
      "She's dead, of course!\n"

continuation :: Animal -> String
continuation Fly =
  "I don't know why she swallowed the fly.\
  \ Perhaps she'll die.\n"
continuation a =
  "She swallowed the " ++ animalToString a
    ++ " to catch the "
    ++ (animalToString' (pred a))
    ++ ".\n"

animalToString :: Animal -> String
animalToString a = fmap toLower (show a)

animalToString' :: Animal -> String
animalToString' Spider =
  "spider that wriggled and jiggled and tickled inside her"
animalToString' a = animalToString a
