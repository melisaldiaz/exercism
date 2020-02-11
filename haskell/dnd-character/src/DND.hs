module DND
  ( Character(..)
  , ability
  , modifier
  , character
  ) where

import           Control.Monad   (replicateM)
import           Data.List       (sort)
import qualified Test.QuickCheck as QC

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  } deriving (Show, Eq)

modifier :: Int -> Int
modifier n = div (n - 10) 2

ability :: QC.Gen Int
ability = fmap (sum . drop 1 . sort) (replicateM 4 dice)

character :: QC.Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  pure
    (Character
       { strength = str
       , dexterity = dex
       , constitution = con
       , intelligence = int
       , wisdom = wis
       , charisma = cha
       , hitpoints = 10 + modifier con
       })

dice :: QC.Gen Int
dice = QC.choose (1, 6)
