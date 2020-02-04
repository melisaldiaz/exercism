{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Garden
  ( Plant(..)
  , garden
  , lookupPlants
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

data Garden =
  Garden (Map String [Plant])

garden :: [String] -> String -> Garden
garden students plants =
  let ss :: [(Int, String)]
      ss = zip [0 ..] students
      ps :: [String]
      ps = lines plants
   in Garden (Map.fromList (fmap (flip plantOwner ps) ss))

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden g) = Map.findWithDefault [] student g

-- Given a student and a list of plants, it pairs
-- the owner with the correct plants.
plantOwner :: (Int, String) -> [String] -> (String, [Plant])
plantOwner student plants =
  case sequence (fmap toPlant (concat (fmap f plants))) of
    Nothing -> (snd student, [])
    Just p  -> (snd student, p)
  where
    f :: String -> String
    f x = take 2 (drop (fst student * 2) x)

-- Given a Char, it returns a Plant, if possible.
toPlant :: Char -> Maybe Plant
toPlant =
  \case
    'C' -> Just Clover
    'G' -> Just Grass
    'R' -> Just Radishes
    'V' -> Just Violets
    _ -> Nothing
