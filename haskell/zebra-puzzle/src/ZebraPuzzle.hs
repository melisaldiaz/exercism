{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZebraPuzzle
  ( Resident (..),
    Solution (..),
    solve,
  )
where

import Control.Monad (guard)
import Data.List (permutations, zipWith5)

data Solution
  = Solution
      { waterDrinker :: Resident,
        zebraOwner :: Resident
      }
  deriving (Eq, Show)

solve :: Solution
solve =
  let s = solve'
      h1 = filter water s
      h2 = filter zebra s
   in case (h1, h2) of
        (a : _, b : _) ->
          Solution (resident a) (resident b)
        _ -> 
          error "Not possible."

-- Only generates permutations of types that match the description
-- of the problem and then discards invalid Houses.
solve' :: [House]
solve' = do
  pets :: [Pet] <- petPerms
  cigs :: [Cigarette] <- cigarettePerms
  guard (nextTo chesterfieldsFox cigs pets)
  guard (nextTo koolsHorse cigs pets)
  ress :: [Resident] <- residentPerms
  cols :: [Color] <- colorPerms
  bevs :: [Beverage] <- beveragePerms
  let houses :: [[House]]
      houses = chunksOf 5 $ zipWith5 House ress cols bevs cigs pets
  hs <- houses
  guard (any englishmanRed hs)
  guard (any spaniardDog hs)
  guard (any greenCoffee hs)
  guard (any ukrainianTea hs)
  guard (any snailsOldGold hs)
  guard (any yellowKools hs)
  guard (any orangeJuiceLuckyStrike hs)
  guard (any japaneseParliaments hs)
  mconcat houses

----------------------------------------------------------
-- TYPES

data House
  = House
      { resident :: Resident,
        color :: Color,
        beverage :: Beverage,
        cigarette :: Cigarette,
        pet :: Pet
      }
  deriving (Show)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Beverage = Water | Tea | Milk | OrangeJuice | Coffee
  deriving (Eq, Show, Enum, Bounded)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

data Pet = Zebra | Dog | Fox | Horse | Snails
  deriving (Eq, Show, Enum, Bounded)

----------------------------------------------------------
-- PREDICATES

-- The Englishman lives in the red house.
englishmanRed :: House -> Bool
englishmanRed h = resident h == Englishman && color h == Red

-- The Spaniard owns the dog.
spaniardDog :: House -> Bool
spaniardDog h = resident h == Spaniard && pet h == Dog

-- Coffee is drunk in the green house.
greenCoffee :: House -> Bool
greenCoffee h = color h == Green && beverage h == Coffee

-- The Ukrainian drinks tea.
ukrainianTea :: House -> Bool
ukrainianTea h = resident h == Ukrainian && beverage h == Tea

-- The green house is immediately to the right of the ivory house.
-- TAKEN CARE BY PERMUATATIONS

-- The Old Gold smoker owns snails.
snailsOldGold :: House -> Bool
snailsOldGold h = pet h == Snails && cigarette h == OldGold

-- Kools are smoked in the yellow house.
yellowKools :: House -> Bool
yellowKools h = color h == Yellow && cigarette h == Kools

-- Milk is drunk in the middle house.
-- TAKEN CARE BY PERMUATATIONS

-- The Norwegian lives in the first house.
-- TAKEN CARE BY PERMUATATIONS

-- The man who smokes Chesterfields lives in the house next to the man with the fox.
chesterfieldsFox :: (Cigarette, Pet) -> Bool
chesterfieldsFox (c, p) = c == Chesterfields && p == Fox

-- Kools are smoked in the house next to the house where the horse is kept.
koolsHorse :: (Cigarette, Pet) -> Bool
koolsHorse (c, p) = c == Kools && p == Horse

-- The Lucky Strike smoker drinks orange juice.
orangeJuiceLuckyStrike :: House -> Bool
orangeJuiceLuckyStrike h = beverage h == OrangeJuice && cigarette h == LuckyStrike

-- The Japanese smokes Parliaments.
japaneseParliaments :: House -> Bool
japaneseParliaments h = resident h == Japanese && cigarette h == Parliaments

-- The Norwegian lives next to the blue house.
-- TAKEN CARE BY PERMUATATIONS

-- Creates a list of tuples from the input lists to obtain adjacent occurrences of elements
-- and performs the given predicate on them. If the list of tuples contains at least one
-- tuple that passes the predicate, it returns True.
nextTo :: forall a b. ((a, b) -> Bool) -> [a] -> [b] -> Bool
nextTo p as bs = any p pairs
  where
    pairs :: [(a, b)]
    pairs = (zip as (drop 1 bs)) ++ (zip (drop 1 as) bs)

-- waterDrinker
water :: House -> Bool
water h = beverage h == Water

-- zebraOwner
zebra :: House -> Bool
zebra h = pet h == Zebra

----------------------------------------------------------
-- PERMUTATIONS

-- The Norwegian lives in the first house.
residentPerms :: [[Resident]]
residentPerms = do
  perm@(Norwegian : _) <- permutations [minBound .. maxBound]
  pure perm

-- The Green house is immediately to the right of the Ivory house.
-- The second house is Blue.
colorPerms :: [[Color]]
colorPerms = do
  perm@(_ : Blue : _) <- permutations [minBound .. maxBound]
  guard (elem (Ivory, Green) (zip perm (drop 1 perm)))
  pure perm

-- Milk is drunk in the middle house.
beveragePerms :: [[Beverage]]
beveragePerms = do
  perm@(_ : _ : Milk : _) <- permutations [minBound .. maxBound]
  pure perm

petPerms :: [[Pet]]
petPerms = permutations [minBound .. maxBound]

cigarettePerms :: [[Cigarette]]
cigarettePerms = permutations [minBound .. maxBound]

----------------------------------------------------------

chunksOf :: forall a. Int -> [a] -> [[a]]
chunksOf n list =
  let f :: [a] -> [[a]]
      f x =
        case splitAt n x of
          (a, b)
            | null a -> []
            | otherwise -> a : (f b)
   in f list
