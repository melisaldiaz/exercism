{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Yacht
  ( yacht
  , Category(..)
  ) where

import           Data.List (break, nub, nubBy, sort, drop)

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice =
  case category of
    Ones           -> numberOf 1 dice
    Twos           -> numberOf 2 dice
    Threes         -> numberOf 3 dice
    Fours          -> numberOf 4 dice
    Fives          -> numberOf 5 dice
    Sixes          -> numberOf 6 dice
    FullHouse      -> fullHouse dice
    FourOfAKind    -> fourOfaKind dice
    LittleStraight -> straightL dice
    BigStraight    -> straightB dice
    Choice         -> choice dice
    Yacht          -> yachtScore dice

-- Calculates the score for categories ones, twos, threes
-- fours, fives, sixes. Any combination of dice is accepted.
numberOf :: Int -> [Int] -> Int
numberOf n dice = n * length (filter (\x -> x == n) dice)

-- Three of a number and two of another. The score
-- is the total of the dice.
fullHouse :: [Int] -> Int
fullHouse dice
  | length (nub dice) == 2 =
    case sort dice of
      a:_ ->
        case break (\x -> x /= a) (sort dice) of
          (n, _) ->
            if length n == 2 || length n == 3
              then sum dice
              else 0
      _ -> 0
  | otherwise = 0

-- At least four dice showing the same face. The score
-- is the total of the four dice.
fourOfaKind :: [Int] -> Int
fourOfaKind dice
  | length allEqual == 5 = sum (drop 1 dice)
  | length allEqual == 4 = sum allEqual
  | otherwise = 0
  where
    allEqual :: [Int]
    allEqual = (nubBy (\x y -> x /= y) dice)

-- Calculates the score for Little Straight.
-- The combination should [1,2,3,4,5].
straightL :: [Int] -> Int
straightL dice
  | length (nub dice) == 5 =
    if [1..5] == sort dice
      then 30
      else 0
  | otherwise = 0

-- Calculates the score for Big Straight.
-- The combination should [2,3,4,5,6].
straightB :: [Int] -> Int
straightB dice
  | length (nub dice) == 5 =
    if [2..6] == sort dice
      then 30
      else 0
  | otherwise = 0

-- Sums any combination of dice.
choice :: [Int] -> Int
choice dice = sum dice

-- All five dice showing the same face.
yachtScore :: [Int] -> Int
yachtScore dice
  | length (nubBy (\x y -> x /= y) dice) == 5 = 50
  | otherwise = 0
